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
  
  (define build-tunnel : (-> String Void)
    (lambda [scepter-host]
      (define foxpipe : Place (dynamic-place `(submod ,(#%file) foxpipe) 'realize))
      (place-channel-put foxpipe (hash 'sshd-host scepter-host
                                       'host-seen-by-sshd "localhost"
                                       'service-seen-by-sshd (sakuyamon-scepter-port)))
      (hash-set! foxpipes scepter-host foxpipe)))
  
  (define px.filter : (Listof Regexp)
    (list #px"\\S+\\[\\d+\\]:\\s*$" #| empty-messaged log such as Safari[xxx] |#
          #px"taskgated\\[\\d+\\]:" #| all especially "no system signature for unsigned ..." |#))

  (define ~geolocation : (-> String String)
    (lambda [ip]
      (define geo : Maybe-Geolocation (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))])))
  
  (define fold-message : (-> Place-Channel Term-Color Any Void)
    (lambda [digivice msgcolour message]
      (place-channel-put digivice (list msgcolour 'fold message))))
  
  (define print-message : (-> Place-Channel String Any Void)
    (lambda [digivice scepter-host message]
      ;(printf "\033[K") ; clear cursor line
      (cond [(char? message) (fold-message digivice (msgcolor) (heart)) #|TODO: Mac's beating heart might be received as strange char|#]
            [(list? message) (for ([msg (in-list message)]) (print-message digivice scepter-host msg)) #|single-line message is also (list)ed.|#]
            [(string? message) (match (string-split message #px"\\s+request:\\s+")
                                 [(list msg)
                                  (cond [(ormap (lambda [[px : Regexp]] (regexp-match px msg)) px.filter) (fold-message digivice 245 msg)]
                                        [(regexp-match* #px"\\d+(\\.\\d+){3}(?!\\.\\S)" msg)
                                         => (lambda [[ips : (Listof String)]]
                                              (place-channel-put digivice
                                                                 (list (msgcolor)
                                                                       (format "~a~n"
                                                                               (regexp-replaces msg (map (lambda [[ip : String]]
                                                                                                           (list ip (~geolocation ip))) ips))))))]
                                        [else (place-channel-put digivice
                                                                 (list msgcolor
                                                                       (format "~a~n" msg)))])]
                                 [(list msghead reqinfo)
                                  (let ([info (cast (with-input-from-string reqinfo read) HashTableTop)])
                                    (place-channel-put digivice
                                                       (list (msgcolor)
                                                             (format "~a ~a@~a //~a~a #\"~a\" " msghead
                                                                     (hash-ref info 'method) (~geolocation (cast (hash-ref info 'client) String))
                                                                     (hash-ref info 'host #false) (hash-ref info 'uri)
                                                                     (hash-ref info 'user-agent #false))))
                                    (place-channel-put digivice
                                                       (list 245
                                                             (format "~s~n"
                                                                     ((inst foldl Symbol HashTableTop Any Any)
                                                                      (lambda [key [info : HashTableTop]] (hash-remove info key)) info
                                                                      '(method host uri user-agent client))))))])]
            [else (place-channel-put digivice (list 245 (format "Unexpected Message from ~a: ~a(~s)~n" scepter-host message message)))])
      (flush-output (current-output-port))))
  
  (define monitor-main : (-> String * Any)
    (lambda hostnames
      (define digivice : Place (dynamic-place `(submod ,(#%file) izuna) 'digivice))
      (when (sync digivice (wrap-evt (place-dead-evt digivice) (lambda [e] #false)))
        (for-each build-tunnel hostnames)
        (define on-signal : (-> exn Void)
          (lambda [signal]
            (for-each (lambda [[foxpipe : Place]] (place-break foxpipe 'terminate)) (hash-values foxpipes))
            (let wait-foxpipe ()
              (define who (apply sync (hash-map foxpipes (lambda [[host : String] [foxpipe : Place]]
                                                           (wrap-evt (place-dead-evt foxpipe) (const host))))))
              (hash-remove! foxpipes who)
              (place-channel-put digivice (list 'blue (format "~a: Foxpipe has collapsed~n" who)))
              (unless (zero? (hash-count foxpipes)) (wait-foxpipe)))
            (place-break digivice 'terminate)))
        (with-handlers ([exn:break? on-signal])
          (let poll-channel ()
            (match (apply sync/enable-break digivice (hash-values foxpipes))
              [(cons host (vector message)) (print-message digivice (cast host String) message)]
              [(cons host (? flonum? s)) (place-channel-put (cast (hash-ref foxpipes host) Place) (format "idled ~as" s))]
              [(list host 'fail message) (place-channel-put digivice (list 'red (format "~a: ~a~n" host message)))]
              [(list host (? string? figureprint) ...) (place-channel-put digivice (list 'cyan (format "~a: RSA: ~a~n" host figureprint)))]
              [(list host 'notify (? string? fmt) argl ...) (place-channel-put digivice  (list 'blue (format "~a: ~a~n" host (apply format fmt argl))))]
              [(or 'exn:break:terminate #false #|sent by digivice|#)
               (call/ec (lambda [[collapse : Procedure]] (raise (exn:break "terminate break" (current-continuation-marks) collapse))))])
            (poll-channel))))
      (place-wait digivice)))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String String * Any)
                                          (Listof String) (-> String Any) Any))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        {"Use an alternative service <port>." "port"}]])
             (lambda [!flag hostname . other-hosts] (apply monitor-main (cons hostname other-hosts)))
             '{"hostname" "2nd hostname"}
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meanwhile Typed Racket does not support CPonter well, So leave the "Typed C" to FFI itself. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* izuna racket
  (provide (all-defined-out))

  (require racket/draw)

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/termctl.rkt")

  (define window%
    (class object% (super-new)
      (init-field x y width height)

      (define border (newwin height width y x))
      (define screen (newpad border ))
      (wborder border #\| #\| #\- #\- #\+ #\+ #\+ #\+)))
  
  (define digivice
    (lambda [izunad]
      (define stdscr (initscr))
      ((curry plumber-add-flush! (current-plumber))
       (lambda [this]
         (plumber-flush-handle-remove! this)
         (endwin)
         (place-channel-put izunad #false)))

      (call-as-normal-termination
       (thunk (with-handlers ([exn:fail? (lambda [ef] (and (endwin) (raise ef)))])
                (when (and stdscr (raw) (noecho) (wtimeout stdscr 0) (keypad stdscr #true)
                           (idlok stdscr #true) (scrollok stdscr #true))
                  (place-channel-put izunad 'Okay)
                  (wborder stdscr #\| #\| #\- #\- #\+ #\+ #\+ #\+)
                  (define-values [width height] (values (getmaxx stdscr) (getmaxy stdscr)))
                  (when (has_colors)
                    (start_color)
                    (use_default_colors)
                    (let ([C (c-extern 'COLORS)])
                      (for ([c (in-range C)])
                        (init_pair c c 'none)
                        (init_pair (+ c C) 'none c))))

                  (with-handlers ([exn:break? exit])
                    (let recv-match-render-loop ()
                      (match (or (getch) (sync/timeout/enable-break 0.26149 #| Number Thoery: Meissel–Mertens Constant |# izunad))
                        [#\003 #| Ctrl+C |# (place-channel-put izunad 'exn:break:terminate)]
                        [(? false?) (void "No key is pressed!")]
                        [(? char? c) (void (mvwaddwstr stdscr (getcury stdscr) 3 (format "Pressed key: ~a ~a!~n" c (char->integer c)))
                                           (wrefresh stdscr))]
                        [(list color 'fold msg) (wcolor_set stdscr (color-number color))
                                                (wmove stdscr (getcury stdscr) (max (getcurx stdscr) 1))
                                                (whline stdscr  #\= (string-length (~a msg)))
                                                (wrefresh stdscr)]
                        [(list color msg) (wcolor_set stdscr (color-number color))
                                          (mvwaddwstr stdscr (getcury stdscr) 1 (~a msg))
                                          (wrefresh stdscr)
                                          (wstandend stdscr)])
                      (recv-match-render-loop))))))))))

(module* foxpipe racket
  (provide (all-defined-out))

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/foxpipe.rkt")

  (require file/sha1)
  
  (define realize
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
        
        (let catch-send-clear-loop ([delay 0.0])
          (define channel-custodian (make-custodian))
          (define ssh-session (make-parameter #false))
          (define terminate/sendback-if-failed
            (lambda [maybe-exn]
              (when (exn:fail? maybe-exn)
                (place-channel-put izunac (list sshd-host 'fail (exn-message maybe-exn))))
              (with-handlers ([exn? void])
                (when (ssh-session) ;;; libssh2 treats long reason as an error
                  (define reason (if (exn? maybe-exn) (exn-message maybe-exn) (~a maybe-exn)))
                  (custodian-shutdown-all channel-custodian) ;;; This also releases libssh2_channel
                  (foxpipe_collapse (ssh-session) (substring reason 0 (min (string-length reason) 256))))
                (collect-garbage))
              (cond [(exn:break:terminate? maybe-exn) (libssh2_exit)]
                    [else (catch-send-clear-loop (+ (sin (current-inexact-milliseconds)) 1.0))])))
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
              (let recv-match-send-loop ()
                (match (sync/timeout/enable-break timeout /dev/sshdin)
                  [(? false?)
                   (let ([reason (place-channel-put/get izunac (cons sshd-host timeout))])
                     (unless (false? reason)
                       (error 'ssh-channel "foxpipe has to collapse: ~a!" reason)))]
                  [(? input-port?)
                   (match (read /dev/sshdin)
                     [(? eof-object?) (error 'ssh-channel "remote server disconnected!")]
                     [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                (recv-match-send-loop)))))))))
