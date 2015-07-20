#lang typed/racket

(provide (all-defined-out))

(define desc : String "Monitor rsyslogs via SSH Tunnel")

(module+ sakuyamon
  (require/typed racket/generator
                 [sequence->repeated-generator (All [a] (-> (Sequenceof a) (-> a)))])
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/geolocation.rkt")
  
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  
  (define foxpipes : (HashTable String Place) (make-hash))
  (define msgcolor : (-> Term-Color)
    ((inst sequence->repeated-generator Term-Color) (list 123 155 187 159 191 223 255)))
  (define heart : (-> Char)
    ((inst sequence->repeated-generator Char) (list beating-heart# two-heart# sparkling-heart# growing-heart# arrow-heart#)))
  
  (define ~geolocation : (-> String String)
    (lambda [ip]
      (define geo : Maybe-Geolocation (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))])))
  
  (define build-tunnel : (-> String Void)
    (lambda [scepter-host]
      (define foxpipe : Place (dynamic-place `(submod ,(#%file) izuna) 'foxpipe))
      (place-channel-put foxpipe (hash 'sshd-host scepter-host
                                       'host-seen-by-sshd "localhost"
                                       'service-seen-by-sshd (sakuyamon-scepter-port)))
      (hash-set! foxpipes scepter-host foxpipe)))
  
  (define px.filter : (Listof Regexp)
    (list #px"\\S+\\[\\d+\\]:\\s*$" #| empty-messaged log such as Safari[xxx] |#
          #px"taskgated\\[\\d+\\]:" #| all especially "no system signature for unsigned ..." |#))
  
  (define fold-message : (-> Term-Color Any Void)
    (lambda [msgcolour message]
      (printf "\033[s\033[2C\033[38;5;~am~a\033[0m\033[u" msgcolour message)))
  
  (define print-message : (-> String Any Void)
    (lambda [scepter-host message]
      (printf "\033[K") ; clear cursor line
      (cond [(char? message) (fold-message (msgcolor) (heart)) #|TODO: Mac's beating heart might be received as strange char|#]
            [(list? message) (for-each (curry print-message scepter-host) message) #|single-line message is also (list)ed.|#]
            [(string? message) (match (string-split message #px"\\s+request:\\s+")
                                 [(list msg)
                                  (cond [(ormap (lambda [[px : Regexp]] (regexp-match px msg)) px.filter) (fold-message 245 msg)]
                                        [(regexp-match* #px"\\d+(\\.\\d+){3}(?!\\.\\S)" msg)
                                         => (lambda [[ips : (Listof String)]]
                                              (echof #:fgcolor (msgcolor) "~a~n"
                                                     (regexp-replaces msg (map (lambda [[ip : String]] (list ip (~geolocation ip))) ips))))]
                                        [else (echof #:fgcolor msgcolor "~a~n" msg)])]
                                 [(list msghead reqinfo)
                                  (let ([info (cast (with-input-from-string reqinfo read) HashTableTop)])
                                    (echof #:fgcolor (msgcolor) "~a ~a@~a //~a~a #\"~a\" " msghead
                                           (hash-ref info 'method) (~geolocation (cast (hash-ref info 'client) String))
                                           (hash-ref info 'host #false) (hash-ref info 'uri)
                                           (hash-ref info 'user-agent #false))
                                    (echof #:fgcolor 245 "~s~n"
                                           ((inst foldl Symbol HashTableTop Any Any)
                                            (lambda [key [info : HashTableTop]] (hash-remove info key)) info
                                            '(method host uri user-agent client))))])]
            [else (echof #:fgcolor 245 "Unexpected Message from ~a: ~a(~s)~n" scepter-host message message)])
      (flush-output (current-output-port))))
  
  (define monitor-main : (-> String * Any)
    (lambda hostnames
      (for-each build-tunnel hostnames)
      (define on-signal : (-> exn Void)
        (lambda [signal]
          (newline)
          (for-each (lambda [[foxpipe : Place]] (place-break foxpipe 'terminate)) (hash-values foxpipes))
          (let wait-channel ()
            (define who (apply sync (hash-map foxpipes (lambda [[host : String] [foxpipe : Place]]
                                                         (wrap-evt (place-dead-evt foxpipe) (const host))))))
            (hash-remove! foxpipes who)
            (echof #:fgcolor 'blue "~a: Foxpipe has collapsed~n" who)
            (unless (zero? (hash-count foxpipes)) (wait-channel)))))
      (with-handlers ([exn:break? on-signal])
        (let poll-channel ()
          (match (apply sync/enable-break (hash-values foxpipes))
            [(cons host (vector message)) (print-message (cast host String) message)]
            [(cons host (? flonum? s)) (place-channel-put (cast (hash-ref foxpipes host) Place) (format "idled ~as" s))]
            [(list host 'fail message) (echof #:fgcolor 'red "~a: ~a~n" host message)]
            [(list host (? string? figureprint) ...) (echof #:fgcolor 'cyan "~a: RSA: ~a~n" host figureprint)]
            [(list host 'notify (? string? fmt) argl ...) (echof #:fgcolor 'blue "~a: ~a~n" host (apply format fmt argl))])
          (poll-channel)))))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String * Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        {"Use an alternative service <port>." "port"}]])
             (lambda [!flag . hostnames]
               (apply monitor-main hostnames)
               ;(place-wait (dynamic-place `(submod ,(#%file) izuna) 'digivice))
               (void))
             '{"hostname"} ;;; Although it can watch multihosts at the same time, but this usage is not recommended due to poor (sync)
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Meanwhile Typed Racket does not support CPonter well
;;; So leave the "Typed C" to FFI itself.

(module* izuna racket
  (provide (all-defined-out))

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/termctl.rkt")
  (require "../../digitama/foxpipe.rkt")

  (require file/sha1)
  
  (define digivice
    (lambda [izunad]
      (initscr)
      (raw)
      (noecho)
      (keypad stdscr #true)
      (wprintw stdscr "Hello, NCurses.\n")
      (wrefresh stdscr)
      (sync /dev/stdin)
      (wprintw stdscr (format "~s~n" (read)))
      (wrefresh stdscr)
      (sleep 2)
      (endwin)))

  (define foxpipe
    (lambda [izunac]
      (with-handlers ([exn:break? void])
        (define time0 (current-inexact-milliseconds))
        (define argh (place-channel-get izunac))
        (match-define (list sshd-host host-seen-by-sshd service-seen-by-sshd)
          (map (curry hash-ref argh) '(sshd-host host-seen-by-sshd service-seen-by-sshd)))
        (match-define (list username passphrase rsa.pub id_rsa)
          (map (curry hash-ref argh)
               '(username passphrase rsa.pub id_rsa)
               (list (current-tamer) ""
                     (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")
                     (build-path (find-system-path 'home-dir) ".ssh" "id_rsa"))))
        
        (let on-sighup ([delay 0.0])
          (define channel-custodian (make-custodian))
          (define ssh-session (make-parameter #false))
          (define terminate/sendback-if-failed
            (lambda [maybe-exn]
              (when (exn:fail? maybe-exn)
                (place-channel-put izunac (list sshd-host 'fail (exn-message maybe-exn))))
              (with-handlers ([exn? displayln])
                (when (ssh-session) ;;; libssh2 treats long reason as an error
                  (define reason (if (exn? maybe-exn) (exn-message maybe-exn) (~a maybe-exn)))
                  (custodian-shutdown-all channel-custodian) ;;; This also releases libssh2_channel
                  (foxpipe_collapse (ssh-session) (substring reason 0 (min (string-length reason) 256))))
                (collect-garbage))
              (cond [(exn:break:terminate? maybe-exn) (libssh2_exit)]
                    [else (on-sighup (+ (sin (current-inexact-milliseconds)) 1.0))])))
          (with-handlers ([exn? terminate/sendback-if-failed])
            (sync/enable-break (alarm-evt (+ (current-inexact-milliseconds) (* (abs delay) 1000))))
            (place-channel-put izunac (list sshd-host 'notify "Connecting to sshd:~a." 22))
            (define session (foxpipe_construct sshd-host 22))
            (ssh-session session)
            (define figureprint (foxpipe_handshake session 'LIBSSH2_HOSTKEY_HASH_SHA1))
            (place-channel-put izunac (cons sshd-host (regexp-match* #px".." (string-upcase (bytes->hex-string figureprint)))))
            (foxpipe_authenticate session username rsa.pub id_rsa passphrase)
            (parameterize ([current-custodian channel-custodian])
              (define timeout (+ (sakuyamon-foxpipe-idle) (/ (- (current-inexact-milliseconds) time0) 1000.0)))
              (define-values [/dev/sshdin /dev/sshdout] (foxpipe_direct_channel session host-seen-by-sshd service-seen-by-sshd))
              (let wait-sshd ()
                (match (sync/timeout/enable-break timeout /dev/sshdin)
                  [(? false?)
                   (let ([reason (place-channel-put/get izunac (cons sshd-host timeout))])
                     (unless (false? reason)
                       (error 'ssh-channel "foxpipe has to collapse: ~a!" reason)))]
                  [(? input-port?)
                   (match (read /dev/sshdin)
                     [(? eof-object?) (error 'ssh-channel "remote server disconnected!")]
                   [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                (wait-sshd)))))))))
