#lang typed/racket

(provide (all-defined-out))

(define desc : String "Monitor rsyslogs via SSH Tunnel")

(module+ sakuyamon
  (require/typed racket/generator
                 [sequence->repeated-generator (All [a] (-> (Sequenceof a) (-> a)))])

  (require "../../digitama/digicore.rkt")
  (require (submod "../../digitama/syslog.rkt" typed))
  
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  
  (define foxpipes : (HashTable String Place) (make-hash))
  (define msgcolor : (-> Term-Color)
    ((inst sequence->repeated-generator Term-Color) (list 123 155 187 159 191 223 255)))
  
  (define build-tunnel : (-> String Void)
    (lambda [scepter-host]
      (define foxpipe : Place (dynamic-place (cast `(submod ,(#%file) foxpipe) Module-Path) 'realize))
      (place-channel-put foxpipe (hash 'sshd-host scepter-host
                                       'host-seen-by-sshd "localhost"
                                       'service-seen-by-sshd (sakuyamon-scepter-port)))
      (hash-set! foxpipes scepter-host foxpipe)))
  
  (define filter-syslog : (-> String Syslog (U (Pairof Symbol String) (Pairof String Syslog)))
    (lambda [scepter-host record]
      (match record
        [(syslog _ _ _ _ appname pid #false) (cons 'Ignore (format "Empty Message from ~a[~a]@~a~n" appname pid scepter-host))]
        [(syslog _ _ _ _ "taskgated" _ unsigned-signature) (cons 'Folded (~a unsigned-signature))]
        [(struct syslog _) (cons scepter-host record)])))
  
  (define monitor-main : (-> Place (Listof String) Any)
    (lambda [digivice hostnames]
      (place-channel-put digivice hostnames)
      (for-each build-tunnel hostnames)
      (define rich-echo (curry place-channel-put digivice))
      (define (on-signal [signal : exn]) : Void
        (rich-echo (cons 'MoreMsg "Terminating Foxpipes"))
        (for-each (lambda [[foxpipe : Place]] (place-break foxpipe 'terminate)) (hash-values foxpipes))
        (let wait-foxpipe ()
          (define who (apply sync (hash-map foxpipes (lambda [[host : String] [foxpipe : Place]] (wrap-evt (place-dead-evt foxpipe) (const host))))))
          (hash-remove! foxpipes who)
          (rich-echo (cons 'Comment (format "Foxpipe@~a has collapsed." who)))
          (unless (zero? (hash-count foxpipes)) (wait-foxpipe)))
        (place-channel-put digivice (exn-message signal)) #| TODO: why (place-break) does not terminate digivice |#)
      (with-handlers ([exn:break? on-signal]) ;;; System Signal also be caught here rather then at place
        (let poll-channel () #| TODO: Mac's heart beat might be received as a strange char |#
          (with-handlers ([exn:fail? (lambda [[e : exn]] (rich-echo (cons 'ErrorMsg (string-join (string-split (exn-message e))))))])
            (match (apply sync/enable-break digivice (wrap-evt (place-dead-evt digivice) (lambda [e] 'exn:break:terminate)) (hash-values foxpipes))
              [(cons host (vector (? char? heart-beat))) (rich-echo (cons host heart-beat))]
              [(cons host (vector (? string? message))) (rich-echo (filter-syslog (cast host String) (string->syslog message)))]
              [(cons host (vector message)) (rich-echo (cons 'WarningMsg (format "Unexpected Message from ~a: ~s" host message)))]
              [(cons host (? flonum? s)) (place-channel-put (cast (hash-ref foxpipes host) Place) (format "idled ~as" s))]
              [(list host (? string? figureprint) ...) (rich-echo (cons host figureprint))]
              [(list 'notify (? string? fmt) argl ...) (rich-echo (cons 'host% (apply format fmt argl)))]
              [(list 'fail (? string? fmt) argl ...) (rich-echo (cons 'WarningMsg (apply format fmt argl)))]
              ['exn:break:terminate #| sent by digivice or digivice is dead |#
               (call/ec (lambda [[collapse : Procedure]] (raise (exn:break "digivice has shutdown" (current-continuation-marks) collapse))))]))
          (poll-channel)))
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
             (lambda [!flag hostname . other-hosts]
               (define digivice : Place (dynamic-place (cast `(submod ,(#%file) izuna) Module-Path) 'digivice))
               (define handshake : Any (sync digivice (wrap-evt (place-dead-evt digivice) (lambda [e] "Digivice is broken!"))))
               (when (string? handshake)
                 (place-wait digivice)
                 (error handshake))
               (monitor-main digivice (cons hostname other-hosts)))
             (list "hostname" "2nd hostname")
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meanwhile Typed Racket does not support CPonter well, So leave the "Typed C" to FFI itself. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* izuna racket
  (provide (all-defined-out))

  (require racket/dict)
  
  (require syntax/location)
  
  (require (submod ".."))
  (require "../../digitama/termctl.rkt")
  (require "../../digitama/syslog.rkt")

  (define titlebar (make-parameter #false))
  (define cmdlinebar (make-parameter #false))
  (define windows (make-hash))

  (define color-links (hasheq 'emerg      'ErrorMsg   #| system is unusable |#
                              'alert      'ErrorMsg   #| action must be taken immediately |#
                              'fatal      'ErrorMsg   #| critical conditions |#
                              'error      'ErrorMsg   #| error conditions |#
                              'warning    'WarningMsg #| warning conditions |#
                              'notice     'MoreMsg    #| normal but significant condition |#
                              'info       'String     #| informational |#
                              'debug      'Debug      #| debug-level messages |#
                              'timestamp  'LineNr
                              'method     'Operator
                              'client     'Tag
                              'host       'StorageClass
                              'uri        'Underlined
                              'referer    'Underlined
                              'user-agent 'Structure))

  (define window%
    (class object% (super-new)
      (field [monitor (newwin 0 0 0 0)])
      (field [statusbar (newwin 1 0 0 0)])

      (define/public (set-status #:update? [update? #true] #:color-pair [color #false] #:offset [x 0] #:width [width (getmaxx statusbar)] . contents)
        (unless (empty? contents) (mvwaddwstr statusbar 0 x (apply ~a contents #:width width)))
        (unless (false? color) (mvwchgat statusbar 0 x width 'StatusLineNC color))
        (when update? (wrefresh statusbar)))
      
      (define/public (resize y x lines cols)
        (wresize monitor lines cols)
        (mvwin monitor y x)
        (wresize statusbar 1 cols)
        (mvwin statusbar (+ y lines) x)
        (wbkgdset statusbar 'StatusLine)
        (set-status (object-name this) #:update? #false)
        (refresh #:update? #false))

      (define/public (refresh #:update? [update? #true])
        (define smart-refresh (if update? wrefresh wnoutrefresh))
        (smart-refresh monitor)
        (smart-refresh statusbar))))

  (define host%
    (class window% (super-new)
      (inherit-field monitor statusbar)
      (inherit set-status resize refresh)

      (define figure-print (box #false))
      (define hosts (make-hash))

      (define/public (add-host! sceptor-host figureprint)
        (set-box! figure-print figureprint)
        (dict-ref! hosts sceptor-host make-hash)
        (let check-next ([idx (dict-iterate-first hosts)] [y 0])
          (cond [(false? idx) (add-host sceptor-host y)]
                [(string=? (dict-iterate-key hosts idx) sceptor-host) (check-next #false y)]
                [else (check-next (dict-iterate-next hosts idx) (+ (dict-count (dict-iterate-value hosts idx)) y 1))]))
        (beat-heart sceptor-host)
        (refresh #:update? #true))

      (define/public (beat-heart scepter-host)
        (set-status scepter-host #:update? #false)
        (set-status #:offset (random (string-length scepter-host)) #:width 1 #:color-pair 'SpecialChar))
      
      (define/private (add-host hostname y)
        (wmove monitor y 0)
        (winsertln monitor)
        (wattrset monitor 'NameSpace)
        (waddstr monitor hostname))))
  
  (define request%
    (class window% (super-new)
      (inherit-field monitor statusbar)
      (inherit set-status resize refresh)
      (scrollok monitor #true)
      
      (define contents (box null))
      (define fields (list 'timestamp 'method 'host 'uri 'client 'user-agent 'referer))

      (define/public (add-record! scepter-host record)
        (set-box! contents (cons (cons scepter-host record) (unbox contents)))
        (display-request scepter-host (syslog-message record))
        (refresh #:update? #true))

      (define/private (display-request scepter-host req)
        (unless (empty? (cdr (unbox contents))) (waddch monitor #\newline))
        (for ([field (in-list fields)])
          (wattrset monitor (hash-ref color-links field))
          (waddstr monitor (request-ref req field))
          (wstandend monitor)
          (waddch monitor #\space)))

      (define/private (request-ref req key)
        (case key
          [(timestamp) (log:request-timestamp req)]
          [else (hash-ref (log:request-headers req) key (const #false))]))))

  (define rsyslog%
    (class window% (super-new)
      (inherit-field monitor statusbar)
      (inherit set-status resize refresh)
      (scrollok monitor #true)
      
      (define contents (box null))
      
      (define/public (add-record! scepter-host record)
        (set-box! contents (cons (cons scepter-host record) (unbox contents)))
        (wattrset monitor (hash-ref color-links (syslog-severity record)))
        (waddwstr monitor (~syslog scepter-host record))
        (refresh #:update? #true))

      (define/private (~syslog scepter-host record)
        (~a #:max-width (getmaxx monitor)
            (format "~a<~a.~a>~a ~a ~a[~a]: ~a"
                    (if (empty? (cdr (unbox contents))) "" #\newline)
                    (syslog-facility record)
                    (syslog-severity record)
                    (syslog-timestamp record)
                    scepter-host
                    (syslog-sender record)
                    (syslog-pid record)
                    (syslog-message record))))))

  (define on-resized
    (lambda []
      (define-values [columns rows] (values (c-extern 'COLS _int) (c-extern 'LINES _int)))
      (define-values [host-cols rsyslog-rows] (values (exact-round (* columns 0.16)) (exact-round (* rows 0.24))))
      (define-values [request-cols request-rows] (values (- columns host-cols 1) (- rows rsyslog-rows 1 1)))
      (for-each (lambda [stdwin] (and (wclear (stdwin)) (wnoutrefresh (stdwin)))) (list stdscr titlebar cmdlinebar))
      (wattrset (titlebar) 'TabLineFill)
      (mvwaddstr (titlebar) 0 0 (~a (current-digimon) #\space (last (quote-module-path)) #:width columns))
      (for ([wtype (in-list (list host%       request%         rsyslog%))]              ; /+-----------+----------------------------------------+\
            [scrny (in-list (list 0           0                (add1 request-rows)))]   ;  | host%     | request%                               |
            [scrnx (in-list (list 0           (add1 host-cols) (add1 host-cols)))]      ;  |           |                                        |
            [scrnr (in-list (list (sub1 rows) request-rows     rsyslog-rows))]          ;  |           |                                        |
            [scrnc (in-list (list host-cols   request-cols     request-cols))])         ;  |           |                                        |
        (define make-wtype (lambda [] (make-object wtype)))                             ;  |           |                                        |
        (define win% (hash-ref! windows (object-name wtype) make-wtype))                ;  |           +----------------------------------------+
        (send win% resize scrny scrnx scrnr scrnc))                                     ;  |           | rsyslog%                               |
      (attrset 'VertSplit)                                                              ;  |           |                                        |
      (mvvline 0 host-cols (- rows 1))                                                  ;  |           |                                        |
      (mvaddch (sub1 rows) host-cols (acs_map 'DARROW #:extra-attrs (list 'underline))) ;  +-----------+----------------------------------------+
      (standend)                                                                        ;  | commandline%, this is a ripped off line.           |
      (for-each wnoutrefresh (list (stdscr) (titlebar)))                                ; \+----------------------------------------------------+/
      (doupdate)))
  
  (define hiecho
    (lambda [higroup fmt . contents]
      (define stdwin (cmdlinebar))
      (wclear stdwin)
      (wattrset stdwin higroup)
      (mvwaddstr stdwin 0 0 (apply format fmt contents))
      (wrefresh stdwin)))
  
  (define digivice
    (lambda [izunad]
      (call-as-normal-termination
       (thunk (dynamic-wind (thunk (and (ripoffline +1 titlebar)
                                        (ripoffline -1 cmdlinebar)
                                        (initscr)))
                            (thunk (with-handlers ([exn:fail? (lambda [e] (place-channel-put izunad (exn-message e)))])
                                     (unless (and (titlebar) (cmdlinebar) (stdscr) (curs_set 0)
                                                  (raw) (noecho) (timeout 0) (intrflush #true) (keypad #true))
                                       (error "NCurses is unavailable!"))
                                     (when (has_colors)
                                       (start_color)
                                       (use_default_colors)
                                       (:colorscheme! (build-path (digimon-stone) "colors.vim")))
                                     (digivice-main izunad (place-channel-put/get izunad 'Okay))))
                            (thunk (endwin)))))))

  (define digivice-main
    (lambda [izunad hostnames]
      (on-resized)
      (with-handlers ([exn:break? (lambda [e] (exit (hiecho 'ErrorMsg "Exit: ~a" (exn-message e))))])
        (let recv-match-render-loop ()
          (with-handlers ([exn:fail? (lambda [e] (hiecho 'ErrorMsg "~a" (string-join (string-split (exn-message e)))))])
            (match (or (getch) (sync/timeout/enable-break 0.26149 #| Number Thoery: Meissel–Mertens Constant |# izunad))
              [(? false?) (void "No key is pressed!")]
              [(cons host (? char?)) (send (hash-ref windows 'host%) beat-heart host)]
              [(cons host (and (syslog _ _ _ _ _ _ (struct log:request _)) record)) (send (hash-ref windows 'request%) add-record! host record)]
              [(cons host (and (struct syslog _) record)) (send (hash-ref windows 'rsyslog%) add-record! host record)]
              [(or 'SIGINT 'SIGQUIT) (place-channel-put izunad 'exn:break:terminate)]
              [(or 'SIGWINCH) (on-resized)]
              [(? char? c) (hiecho 'Ignore "Key pressed: ~s[~a]" c (char->integer c))]
              [(cons 'host% notify) (send (hash-ref windows 'host%) set-status notify)]
              [(cons (? string? host) figureprint) (send (hash-ref windows 'host%) add-host! host figureprint)]
              [(cons (? symbol? group) message) (hiecho group message)]
              [reason (call/ec (lambda [collapse] (raise (exn:break (~a reason) (current-continuation-marks) collapse))))]))
          (recv-match-render-loop))))))

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
                (place-channel-put izunac (list 'fail "~a: ~a" sshd-host (exn-message maybe-exn))))
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
            (place-channel-put izunac (list 'notify "Connecting to ~a:~a." sshd-host 22))
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
