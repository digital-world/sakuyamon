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
        (place-channel-put digivice hostnames)
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

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/termctl.rkt")

  (define vim-cterm-colors
    #hash(("black" . 0) ("darkgray" . 8) ("darkgrey" . 8) ("lightgray" . 7) ("lightgrey" . 7) ("gray" . 7) ("grey" . 7) ("white" . 15)
                        ("darkred" . 1) ("darkgreen" . 2) ("darkyellow" . 3) ("darkblue" . 4) ("brown" . 5) ("darkmagenta" . 5) ("darkcyan" . 6)
                        ("red" . 9) ("lightred" . 9) ("green" . 10) ("lightgreen" . 10) ("yellow" . 11) ("lightyellow" . 11)
                        ("blue" . 12) ("lightblue" . 12) ("magenta" . 13) ("lightmagenta" . 13) ("cyan" . 14) ("lightcyan" . 14)))
  
  (define vim-cterm-attrs
    #hash(("none" . normal) ("standout" . standout) ("underline" . underline) ("undercurl" . undercurl)
                            ("reverse" . reverse) ("inverse" . reverse) ("blink" . blink) ("bold" . bold)))

  (struct color-pair (index cterm ctermfg ctermbg) #:prefab)
  (define vim-highlight->ncurses-color-pair
    (lambda [colors.vim]
      (define highlight (make-hash))
      (when (file-exists? colors.vim)
        (with-input-from-file colors.vim
          (thunk (for ([line (in-port read-line)]
                       #:when (< (hash-count highlight) 256)
                       #:when (regexp-match? #px"c?term([fb]g)?=" line))
                   (define-values [attrs head] (partition (curry regexp-match #px"=") (string-split line)))
                   (match-define (list cterm ctermfg ctermbg) (map make-parameter (list null 'none 'none)))
                   (for ([token (in-list attrs)])
                     (match token
                       [(pregexp #px"ctermfg=(\\d+)" (list _ nfg)) (ctermfg (min 256 (string->number nfg)))]
                       [(pregexp #px"ctermfg=(\\D+)" (list _ fg)) (ctermfg (hash-ref vim-cterm-colors (string-downcase fg) (const 'none)))]
                       [(pregexp #px"ctermbg=(\\d+)" (list _ nbg)) (ctermbg (min 256 (string->number nbg)))]
                       [(pregexp #px"ctermbg=(\\D+)" (list _ bg)) (ctermbg (hash-ref vim-cterm-colors (string-downcase bg) (const 'none)))]
                       [term (cterm (filter-map (lambda [a] (hash-ref vim-cterm-attrs (string-downcase a) #false))
                                                (map string-downcase (cdr (string-split term #px"[=,]")))))]))
                   (hash-set! highlight (string->symbol (last head)) (color-pair (add1 (hash-count highlight)) (cterm) (ctermfg) (ctermbg)))))))
      highlight))

  
  #| +---------------+----------------------------------------+
     | hosts%        | request%                               |
     |               |                                        |
     |               |                                        |
     |               |                                        |
     |               |                                        |
     |               +----------------------------------------+
     |               | rsyslog%                               |
     |               |                                        |
     |               |                                        |
     +---------------+----------------------------------------+
     | commandline%                                           |
     +--------------------------------------------------------+ |#

  (define host% (make-parameter #false))
  (define request% (make-parameter #false))
  (define rsyslog% (make-parameter #false))
  (define commandline% (make-parameter #false))

  (define create-window
    (lambda [type% row column y x label vline?]
      (define stdwin (newwin row column y x))
      (unless (false? stdwin)
        (when (type%) (delwin (type%)))
        (type% stdwin)
        (when vline?
          (mvwvline stdwin 0 (sub1 column) row))
        (when label
          (mvwaddwstr stdwin (sub1 row) 0 (~a label #:max-width column))
          (mvwchgat stdwin (sub1 row) 0 -1 (list 'underline) 0)
          (wmove stdwin 0 0)))))
  
  (define on-resized
    (lambda [stdscr]
      (define-values [columns rows] (values (getmaxx stdscr) (getmaxy stdscr)))
      (define-values [host-width commandline-height rsyslog-height] (values 32 1 8))
      (define-values [request-width request-height] (values (- columns host-width) (- rows commandline-height rsyslog-height)))
      (create-window host% (- rows commandline-height) host-width 0 0 'host #true)
      (create-window request% request-height request-width 0 host-width 'request #false)
      (create-window rsyslog% rsyslog-height request-width request-height host-width 'rsyslog #false)
      (create-window commandline% commandline-height columns rows 0 #false #false)
      (for-each wrefresh (list stdscr (host%) (request%) (rsyslog%) (commandline%)))))

  (define mvwaddhistr
    (lambda [stdwin y x info fmt . contexts]
      (init_pair (color-pair-index info) (color-pair-ctermfg info) (color-pair-ctermbg info))
      (wattr_on stdwin (color-pair-cterm info))
      (wcolor_set stdwin (color-pair-index info))
      (mvwaddwstr stdwin y x (apply format fmt contexts))
      (wstandend stdwin)))
  
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
                (when (and stdscr (raw) (noecho) (wtimeout stdscr 0) (keypad stdscr #true) (idlok stdscr #true) (scrollok stdscr #true))
                  (define highlight (vim-highlight->ncurses-color-pair (build-path (digimon-stone) "colors.vim")))
                  (when (has_colors)
                    (start_color)
                    (use_default_colors)
                    (for ([[name group] (in-hash highlight)])
                      (init_pair (color-pair-index group) (color-pair-ctermfg group) (color-pair-ctermbg group))))
                
                  (define hostnames (place-channel-put/get izunad 'Okay))
                  (on-resized stdscr)
                  
                  (with-handlers ([exn:break? exit])
                    (let recv-match-render-loop ()
                      (match (or (wgetch stdscr) (sync/timeout/enable-break 0.26149 #| Number Thoery: Meissel–Mertens Constant |# izunad))
                        [(? false?) (void "No key is pressed!")]
                        [(list color 'fold msg) ;(wcolor_set (rsyslog%) (color-number color))
                                                (wmove (commandline%) 0 3)
                                                ;(wrefresh (rsyslog%))
                                                ]
                        [(list color msg) ;(wcolor_set (request%) (color-number color))
                                          (mvwaddwstr (rsyslog%) (getcury (rsyslog%)) 1 (~a msg))
                                          (wrefresh (rsyslog%))
                                          ;(wstandend (request%))
                                          ]
                        [#\003 #| Ctrl+C |# (place-channel-put izunad 'exn:break:terminate)]
                        [#\u19A #| terminal size changed |# (on-resized stdscr)]
                        [(? char? c) (void (mvwaddwstr (commandline%) 0 3 (format "Pressed key: ~a ~a!~n" c (char->integer c)))
                                           (wrefresh (commandline%)))])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test racket
  (require (submod ".." izuna))
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/termctl.rkt")

  (define colors.vim (build-path (digimon-stone) "colors.vim"))
  (define highlight (vim-highlight->ncurses-color-pair colors.vim))
  (when (and (initscr) (has_colors))
    (start_color)
    (use_default_colors)
    (curs_set 0)
    (define uncaught-exn (make-parameter #false))
    (define-values [field-size field-count] (values (+ 12 (apply max (map (compose1 string-length symbol->string) (hash-keys highlight)))) 4))
    (define-values [stdclr-cols stdclr-rows] (values (add1 (* field-count field-size)) (+ 2 (ceiling (/ (hash-count highlight) field-count)))))
    (define stdscr (newwin 0 0 0 0)) ; full screen window, size will auto-change when term has changed size. 
    (define stdclr (newwin stdclr-rows stdclr-cols 0 0))
    ((curry plumber-add-flush! (current-plumber))
     (lambda [this] (let ([e (uncaught-exn)])
                      (plumber-flush-handle-remove! this)
                      (delwin stdscr) (delwin stdclr) (endwin)
                      (when (exn? e) (displayln (exn-message e))))))

    (with-handlers ([exn:fail? uncaught-exn])
      (when (and stdscr stdclr (raw) (noecho) (wtimeout stdscr -1) (keypad stdscr #true) (idlok stdscr #true) (scrollok stdscr #true))
        (let display-colors ()
          (wclear stdscr)
          (mvwaddhistr stdscr 0 0 (hash-ref highlight 'Visual) (~a "> raco test digivice/sakuyamon/izuna.rkt" #:min-width (getmaxx stdscr)))
          (mvwin stdclr (add1 (quotient (- (getmaxy stdscr) stdclr-rows) 2)) (quotient (- (getmaxx stdscr) stdclr-cols) 2))
          (wmove stdclr 0 0)
          (for ([[name group] (in-hash highlight)]
                [index (in-naturals 1)])
            (mvwaddhistr stdclr (getcury stdclr) (getcurx stdclr) group
                         (~a name #\[ (color-pair-ctermfg group) #\, (color-pair-ctermbg group) #\] #:min-width field-size))
            (when (zero? (remainder index field-count)) (waddwstr stdclr (~a #\newline))))
          (mvwaddhistr stdscr (- (getmaxy stdscr) 2) 0 (hash-ref highlight 'StatusLine) (~a "Press any key to exit!" #:min-width (getmaxx stdscr)))
          (wrefresh stdscr)
          (wrefresh stdclr) ; it must be rendered after rendering stdscr
            
          (when (char=? (wgetch stdscr) #\u19A)
            (display-colors)))))))
