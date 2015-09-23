#lang typed/racket

(provide (all-defined-out))

(define desc : String "Monitor rsyslogs via SSH Tunnel")

(module* sakuyamon typed/racket
  (require syntax/location)

  (require (submod ".."))
  (require (submod "../../digitama/posix.rkt" typed/ffi))
  (require (submod "../../digitama/termctl.rkt" typed/ffi))
  (require (submod "../../digitama/foxpipe.rkt" typed/ffi))
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/rfc5424.rkt")
  (require "../../digitama/geolocation.rkt")

  (require/typed/provide racket
                         [vector-set-performance-stats! (-> Racket-Place-Status (Option Thread) Void)])

  (require/typed/provide racket/date
                         [current-date (-> date)] ;;; should be date*
                         [date-display-format (Parameterof Symbol)]
                         [date->string (-> date Boolean String)])
  
  (define-type Racket-Place-Status (Vector Fixnum Fixnum Fixnum Natural Natural Natural Natural Natural Fixnum Fixnum Natural Natural))
  (define-type NCurseWindow<%> (Class (field [monitor Window*] [statusbar Window*])
                                      [resize (-> Natural Natural Positive-Integer Positive-Integer Any)]
                                      [set-status (-> [#:clear? Boolean] [#:color (Option Color-Pair)] [#:offset Natural] [#:width Positive-Integer] Any * Any)]
                                      [refresh (-> [#:update? Boolean] Any)]
                                      [add-record! (-> String Syslog Any)]
                                      [add-host! (-> String (Listof String) Any)]
                                      [beat-heart (-> String Any)]))
  
  (define uptime/base : Fixnum (current-milliseconds))
  (define pr+gctime/base : Fixnum (current-process-milliseconds))
  
  ; /+-----------+----------------------------------------+\
  ;  | title% [this is a ripped off line]                 |
  ;  +-----------+----------------------------------------+
  ;  | host%     | request%                               |
  ;  |           |                                        |
  ;  |           |                                        |
  ;  |           |                                        |
  ;  |           |                                        |
  ;  |           +----------------------------------------+
  ;  |           | rsyslog%                               |
  ;  |           |                                        |
  ;  |           |                                        |
  ;  +-----------+----------------------------------------+
  ;  | commandline% [this is a ripped off line]           |
  ; \+----------------------------------------------------+/
  
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  (define sakuyamon-colors.vim : (Parameterof Path-String) (make-parameter (build-path (digimon-stone) "colors.vim")))
  (define sakuyamon-action : String (path->string (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #"")))

  (define izuna-title : String (~a (current-digimon) #\space sakuyamon-action #\[ (getpid) #\]))
  (define izuna-statistics : Racket-Place-Status (vector 0 0 0 0 0 0 0 0 0 0 0 0))
  (define mtime-colors.vim : (Boxof Natural) (box 0))
  (define px.ip : PRegexp #px"\\d{1,3}(\\.\\d{1,3}){3}")

  (define-strdict foxpipe : Place)
  (define-symdict stdwin : (Instance NCurseWindow<%>))

  (define color-links : (-> (HashTable Symbol Symbol))
    (let* ([colors.rktl : Path-String (build-path (digimon-stone) "colors.rktl")]
           [mtime : (Boxof Natural) (box 0)]
           [links : (Boxof (HashTable Symbol Symbol)) (box ((inst make-hasheq Symbol Symbol)))])
      (lambda []
        (define last-mtime : Natural (file-or-directory-modify-seconds colors.rktl #false (const 0)))
        (when (< (unbox mtime) last-mtime)
          (set-box! mtime last-mtime)
          (set-box! links (with-handlers ([exn? (lambda [e] (unbox links))])
                            (cast (car.eval (with-input-from-file colors.rktl read)) (HashTable Symbol Symbol)))))
        (unbox links))))

  (define ~geolocation : (-> String String * String)
    (lambda [ip . whocares]
      (match (what-is-my-address ip)
        [(geolocation continent country _ #false _ _) (format "~a[~a/~a]" ip continent country)]
        [(geolocation _ country _ city _ _) (format "~a[~a ~a]" ip country city)]
        [false/else/error ip])))

  (define window% : NCurseWindow<%>
    (class object% (super-new)
      (field [monitor (cast (newwin 0 0 0 0) Window*)])
      (field [statusbar (cast (newwin 1 0 0 0) Window*)])
      
      (define/public (set-status #:clear? [clear? #false] #:color [color #false] #:offset [x 0] #:width [width (getmaxx statusbar)] . contents)
        (unless (false? clear?) (wclear statusbar))
        (unless (empty? contents) (mvwaddwstr statusbar 0 x (~a ((inst apply Any String) ~a contents) #:width width)))
        (unless (false? color) (mvwchgat statusbar 0 x width 'StatusLineNC color))
        (wnoutrefresh statusbar))
      
      (define/public (resize y x lines cols)
        (wresize monitor (cast (sub1 lines) Positive-Integer) cols)
        (mvwin monitor y x)
        (wresize statusbar 1 cols)
        (mvwin statusbar (sub1 (+ y lines)) x)
        (wbkgdset statusbar 'StatusLine))
      
      (define/public (refresh #:update? [update? #false])
        (define smart-refresh : (-> Window* Any) (if update? wrefresh wnoutrefresh))
        (smart-refresh monitor)
        (smart-refresh statusbar))

      (define/public (add-host! scepter-host figureprint)
        (raise (exn:fail:unsupported "add-host!: invoking an abstract method!" (current-continuation-marks))))

      (define/public (add-record! scepter-host record)
        (raise (exn:fail:unsupported "add-record!: invoking an abstract method!" (current-continuation-marks))))

      (define/public (beat-heart scepter-host)
        (raise (exn:fail:unsupported "beat-heart!: invoking an abstract method!" (current-continuation-marks))))))
  
  (define host% : NCurseWindow<%>
    (class window% (super-new)
      (inherit-field monitor statusbar)
      (inherit set-status resize refresh add-record!)
      
      (define figure-print : (Boxof (Listof String)) (box null))
      (define hosts : (HashTable String SymbolTable) (make-hash))
      
      (define/override add-host!
        (lambda [sceptor-host figureprint]
          (set-box! figure-print figureprint)
          (unless (hash-has-key? hosts sceptor-host)
            ((inst hash-set! String SymbolTable) hosts sceptor-host (cast (make-hash) SymbolTable))
            (let check-next ([idx : (Option Integer) (hash-iterate-first hosts)]
                             [y : Natural 0])
              (cond [(false? idx) (add-host sceptor-host y)]
                    [(string=? (hash-iterate-key hosts idx) sceptor-host) (check-next #false y)]
                    [else (check-next (hash-iterate-next hosts idx) (+ (hash-count (hash-iterate-value hosts idx)) y 1))]))
            (alert 'MoreMsg "~a: ~a" sceptor-host figureprint))
          (set-status sceptor-host)
          (refresh)))

      (define/override beat-heart
        (lambda [scepter-host]
          (set-status scepter-host)
          (set-status #:offset (random (string-length scepter-host)) #:width 1 #:color 'SpecialChar)))
      
      (define/private (add-host [hostname : String] [y : Natural]) : Boolean
        (wmove monitor y 0)
        (winsertln monitor)
        (wattrset monitor 'NameSpace)
        (waddstr monitor hostname))))
  
  (define request% : NCurseWindow<%>
    (class window% (super-new)
      (inherit-field monitor statusbar)
      (inherit set-status resize refresh add-host! beat-heart)
      (scrollok monitor #true)
      
      (define contents : (Boxof (Listof (Pairof String Syslog))) (box null))
      (define fields : (Listof Symbol) (list 'timestamp 'method 'host 'uri 'client 'user-agent 'referer))
      
      (define/override add-record! : (-> String Syslog Any)
        (lambda [scepter-host record]
          (define req : Log:Message (syslog5424-message record))
          (set-box! contents (cons (cons scepter-host record) (unbox contents)))
          (when (log:request? req)
            (display-request scepter-host req))
          (refresh)))
      
      (define/private (display-request [scepter-host : String] [req : Log:Request]) : Any
        (define hilinks (color-links))
        (unless (empty? (cdr (unbox contents))) (waddch monitor #\newline))
        (for ([field (in-list fields)])
          (wattrset monitor (hash-ref hilinks field (const (list 0))))
          (waddstr monitor (~a (request-ref req field)))
          (wstandend monitor)
          (waddch monitor #\space)))
      
      (define/private (request-ref [req : Log:Request] [key : Symbol]) : (Option String)
        (case key
          [(timestamp) (log:request-timestamp req)]
          [(client) (~geolocation (log:request-client req))]
          [else (hash-ref (log:request-headers req) key (const #false))]))))
  
  (define rsyslog% : NCurseWindow<%>
    (class window% (super-new)
      (inherit-field monitor statusbar)
      (inherit set-status resize refresh add-host! beat-heart)
      (scrollok monitor #true)
      
      (define contents : (Boxof (Listof (Pairof String Syslog))) (box null))
      
      (define/override add-record! : (-> String Syslog Any)
        (lambda [scepter-host record]
          (set-box! contents (cons (cons scepter-host record) (unbox contents)))
          (wattrset monitor (hash-ref (color-links) (syslog5424-severity record) (const (list 0))))
          (waddwstr monitor (~syslog scepter-host record))
          (refresh)))

      (define/private (~syslog [scepter-host : String] [record : Syslog]) : String
        (~a #:max-width (getmaxx monitor)
            (format "~a~a ~a ~a ~a[~a]: ~a"
                    (if (empty? (cdr (unbox contents))) "" #\newline)
                    (~a (syslog5424-facility record) #:width 8)
                    (syslog5424-timestamp record)
                    scepter-host
                    (syslog5424-sender record)
                    (syslog5424-pid record)
                    (regexp-replaces (~a (syslog5424-message record))
                                     `([,px.ip ,~geolocation]
                                       [,(string #\newline) " "]
                                       [,(string #\tab) "    "])))))))

  (define display-statistics : (-> [#:stdbar Window*] [#:prefix String] Any)
    (lambda [#:stdbar [stdbar (:prefabwin 'titlebar)] #:prefix [prefix izuna-title]]
      (match-define (vector pr+gctime/now uptime/now gctime/now gctimes _ _ _ _ _ _ sysmem _)
        (and (vector-set-performance-stats! izuna-statistics #false) izuna-statistics))
      (define uptime : Positive-Fixnum (cast (- uptime/now uptime/base) Positive-Fixnum))
      (define pr+gctime : Positive-Fixnum (cast (- pr+gctime/now pr+gctime/base) Positive-Fixnum))
      (define ~t : (-> Natural Natural String) (lambda [n w] (~r #:min-width w #:pad-string "0" n)))
      (define ~m : (-> Natural String) (lambda [m] (~r #:precision '(= 3) (/ m 1024.0 1024.0))))
      (define ~% : (-> Flonum String) (lambda [%] (~r #:precision '(= 2) (* 100.0 (max 0 %)))))
      (let*-values ([(s) (quotient uptime 1000)]
                    [(d s) (quotient/remainder s 86400)]
                    [(h s) (quotient/remainder s 3600)]
                    [(m s) (quotient/remainder s 60)])
        (define status : String (~a #:align 'right #:width (max 0 (- (getmaxx stdbar) (string-length prefix)))
                                    (format "~a ~a:~a:~a up, ~ams gc[~a], ~a% idle, ~aMB, ~a"
                                            (~n_w d "day") (~t h 2) (~t m 2) (~t s 2) gctime/now gctimes
                                            (~% (- 1.0 (/ pr+gctime uptime))) (~m (+ (current-memory-use) sysmem))
                                            (parameterize ([date-display-format 'iso-8601])
                                              (date->string (current-date) #true)))))
        (mvwaddstr stdbar 0 0 (string-append prefix status))
        (wnoutrefresh stdbar))))
  
  (define alert : (-> Symbol String [#:stdbar Window*] Any * Any)
    (lambda [#:stdbar [stdbar (:prefabwin 'cmdlinebar)] higroup fmt . contents]
      (wclear stdbar)
      (wattrset stdbar higroup)
      (mvwaddstr stdbar 0 0 (~a (regexp-replaces (apply format fmt contents)
                                                 (list (list #px"\\s+" " ")
                                                       (list (path->string (digimon-zone)) "")))))
      (wrefresh stdbar)))
  
  (define update-windows-on-screen : (-> Any)
    (lambda []
      (define mtime : Natural (file-or-directory-modify-seconds (sakuyamon-colors.vim)))
      (when (< (unbox mtime-colors.vim) mtime)
        (:colorscheme! (sakuyamon-colors.vim))
        (set-box! mtime-colors.vim mtime)
        (on-digivice-resized))
      (doupdate)))

  (define on-timer/second : (-> Natural Any)
    (lambda [times]
      (display-statistics)))

  (define on-digivice-resized : (-> Any)
    (lambda []
      (define columns : Positive-Integer (getmaxx (:prefabwin 'stdscr)))
      (define rows : Positive-Integer (getmaxy (:prefabwin 'stdscr)))
      (define host-cols : Positive-Integer (cast (exact-round (* columns 0.16)) Positive-Integer))
      (define rsyslog-rows : Positive-Integer (cast (exact-round (* rows 0.24)) Positive-Integer))
      (define request-cols : Positive-Integer (cast (- columns host-cols 1) Positive-Integer))
      (define request-rows : Positive-Integer (cast (- rows rsyslog-rows) Positive-Integer))
      (for-each wclear (list (:prefabwin 'stdscr) (:prefabwin 'cmdlinebar)))
      (wattrset (:prefabwin 'titlebar) 'Title)
      (display-statistics #:stdbar (:prefabwin 'titlebar) #:prefix izuna-title)
      (for ([win% : NCurseWindow<%>  (in-list (list host%       request%         rsyslog%))]
            [scry : Natural          (in-list (list 0           0                request-rows))]
            [scrx : Natural          (in-list (list 0           (add1 host-cols) (add1 host-cols)))]
            [scrr : Positive-Integer (in-list (list rows        request-rows     rsyslog-rows))]
            [scrc : Positive-Integer (in-list (list host-cols   request-cols     request-cols))])
        (define stdwin : (Instance NCurseWindow<%>) ($stdwin+ (cast (object-name win%) Symbol) (make-object win%)))
        (send stdwin resize scry scrx scrr scrc)
        (send stdwin set-status (object-name win%))
        (send stdwin refresh #:update? #true))
      (attrset 'VertSplit)
      (mvvline 0 host-cols (- rows 1))
      (mvaddch (sub1 rows) host-cols (:altchar 'DARROW #:extra-attrs (list 'underline)))))
  
  (define on-foxpipe-rsyslog : (-> String Syslog Any)
    (lambda [scepter-host record]
      (define (rich-status [colorpair : Color-Pair] [message : String])
        (send ($stdwin 'rsyslog%) set-status #:clear? #true #:color colorpair #:width (max (string-length message) 1) message))
      (match record
        [(syslog5424 _ _ timestamp host app pid #false) (rich-status 'Ignore (format "~a@~a: ~a[~a]: [Empty Message]" timestamp host app pid))]
        [(syslog5424 _ _ timestamp host "taskgated" _ unsigned-signature) (rich-status 'Folded (format "~a@~a: ~a" timestamp host unsigned-signature))]
        [(syslog5424 _ _ _ _ _ _ (? string? weird-log:request-seems-to-be-the-black-hole)) (send ($stdwin 'rsyslog%) add-record! scepter-host record)]
        [(syslog5424 _ _ _ _ _ _ (struct log:request _)) (send ($stdwin 'request%) add-record! scepter-host record)])))
  
  (define digivice-recv-match-render-loop : (-> Any)
    (lambda []
      (with-handlers ([exn:fail? (lambda [[e : exn]] (alert 'ErrorMsg "~a" (exn-message e)))])
        (match (or (getch) (apply sync/timeout/enable-break 0.26149 #| Meissel–Mertens Constant |# ($foxpipe*)))
          [(? false? on-system-idle) (update-windows-on-screen)]
          [(cons (? string? host) (vector (? char? heart-beat))) (send ($stdwin 'host%) beat-heart host)]
          [(cons (? string? host) (vector (? string? message))) (on-foxpipe-rsyslog host (string->syslog message))]
          [(cons (? string? host) (vector message)) (alert 'WarningMsg "Received an unexpected message from ~a: ~s" host message)]
          [(cons (? string? host) (? flonum? s)) (place-channel-put ($foxpipe host) (format "idled ~as" s))]
          [(list (? string? host) (? string? figureprint) ...) (send ($stdwin 'host%) add-host! host (cast figureprint (Listof String)))]
          [(list (? string? host) 'notify (? string? fmt) argl ...) (send ($stdwin 'host%) set-status host ": " (apply format fmt argl))]
          [(list (? string? host) 'fail (? string? fmt) argl ...) (alert 'WarningMsg "~a: ~a" host (apply format fmt argl))]
          [(? char? c) (alert 'Ignore "Key pressed: ~s[~a]" c (char->integer c))]
          [(and (or 'SIGQUIT 'SIGINT) signal) (raise-signal-error signal)]
          [(or 'SIGWINCH) (on-digivice-resized)]))
      (digivice-recv-match-render-loop)))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String String * Void)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) sakuyamon-action)
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [["-c"] ,(lambda [flag [cs.vim : Path-String]] (when (file-exists? cs.vim) (sakuyamon-colors.vim cs.vim)))
                        ["Use an alternative color scheme <colors.vim>." "colors.vim"]]
                [["-p"] ,(lambda [flag [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        ["Use an alternative service <port>." "port"]]])
             (lambda [!flag hostname . other-hosts]
               ((inst dynamic-wind Void)
                (thunk (and (ripoffline +1 'titlebar)
                            (ripoffline -1 'cmdlinebar)
                            (initscr)
                            (libssh2_init)))
                (thunk (with-handlers ([exn:fail? (lambda [[e : exn]] (endwin) (displayln (exn-message e)))])
                         (unless (and (:prefabwin 'titlebar) (:prefabwin 'cmdlinebar) (:prefabwin 'stdscr)
                                      (curs_set 0) (raw) (noecho) (timeout 0) (intrflush #true) (keypad #true))
                           (error "NCurses is unavailable!"))
                         (when (has_colors)
                           (start_color)
                           (use_default_colors))
                         (update-windows-on-screen) ; will load the up-to-date colorscheme
                         (define timer (timer-thread on-timer/second 1.0))
                         (for ([scepter-host : String (in-list (cons hostname other-hosts))])
                           (place-channel-put ($foxpipe+ scepter-host (dynamic-place (cast `(submod ,(#%file) foxpipe) Module-Path) 'realize))
                                              ((inst hasheq Symbol Any)
                                               'sshd-host scepter-host
                                               'host-seen-by-sshd "localhost"
                                               'service-seen-by-sshd (sakuyamon-scepter-port)
                                               'plaintransport? (and (or (member scepter-host '("localhost" "::1"))
                                                                         (regexp-match? #px"^127\\." scepter-host)) #true))))
                         (with-handlers ([exn:break? void]) ;;; All Signals are escaped to allow user killing blocked places
                           (digivice-recv-match-render-loop))
                         (noraw) ;;; ncurses will not stop user signals
                         (alert 'MoreMsg "Terminating Foxpipes")
                         (for-each (lambda [[this : Place]] (place-break this 'terminate)) ($foxpipe*))
                         (with-handlers ([exn:break? void])
                           (let wait-foxpipe ()
                             (unless (zero? ($foxpipe#))
                               (define who : String
                                 (apply sync/enable-break (hash-map %foxpipe (lambda [[host : String] [foxpipe : Place]]
                                                                               (wrap-evt (place-dead-evt foxpipe)
                                                                                         ((inst const String) host))))))
                               (alert 'Comment "Foxpipe@~a has collapsed." who)
                               (!foxpipe- who)
                               (wait-foxpipe))))
                         (for-each place-kill ($foxpipe*))
                         (break-thread timer)))
                (thunk (and (endwin)
                            (libssh2_exit)))))
             (list "hostname" "2nd hostname")
             (lambda [--help]
               (display (string-replace --help #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))

#|===========================================================================================================================|#

(module* foxpipe typed/racket
  (provide (all-defined-out))
  
  (require (submod "../../digitama/posix.rkt" typed/ffi))
  (require (submod "../../digitama/foxpipe.rkt" typed/ffi))

  (require "../../digitama/digicore.rkt")
  
  (require/typed file/sha1
                 [bytes->hex-string (-> Bytes String)])

  (define ssh-session : (Parameterof Foxpipe-Session*/Null) (make-parameter #false))

  (define realize : Place-Main
    (lambda [izunac]
      (with-handlers ([exn:break? void])
        (define/extract-symtable (place-channel-get izunac)
          [sshd-host : String]
          [host-seen-by-sshd : String]
          [service-seen-by-sshd : Index]
          [plaintransport? : Boolean]
          [username : String = (current-tamer)]
          [passphrase : String = ""]
          [rsa.pub : Path-String = (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")]
          [id_rsa : Path-String = (build-path (find-system-path 'home-dir) ".ssh" "id_rsa")])

        (define (on-collapsed [signal : exn]) : Void
          (define session : Foxpipe-Session*/Null (ssh-session))
          (when (exn:fail? signal)
            (place-channel-put izunac (list sshd-host 'fail (exn-message signal))))
          (custodian-shutdown-all (current-custodian))
          (when (foxpipe-session*? session)
            (foxpipe_collapse session (~a signal)))
          (when (exn:break? signal)
            (exit 0)))
        
        (let catch-send-clear-loop : Void ()
          (parameterize ([current-custodian (make-custodian)])
            (with-handlers ([exn? on-collapsed])
              (match-define-values ((list session) cputime wallclock gctime)
                                   ((inst time-apply Foxpipe-Session*/Null Any)
                                    (thunk (collect-garbage)
                                           (when (false? plaintransport?)
                                             (place-channel-put izunac (list sshd-host 'notify "constructing ssh channel"))
                                             (define session : Foxpipe-Session*/Null (foxpipe_construct sshd-host 22 1618))
                                             (when (foxpipe-session*? session)
                                               (ssh-session session)
                                               (define figureprint : String (bytes->hex-string (foxpipe_handshake session 'hostkey_hash_sha1)))
                                               (place-channel-put izunac (cons sshd-host (regexp-match* #px".." (string-upcase figureprint))))
                                               (foxpipe_authenticate session username rsa.pub id_rsa passphrase)))
                                           (ssh-session))
                                    null))
              (define maxinterval : Positive-Real (+ (sakuyamon-foxpipe-idle) (/ (+ cputime gctime) 1000.0)))
              (define-values (/dev/tcpin /dev/tcpout)
                (cond [(foxpipe-session*? session) (foxpipe_direct_channel session host-seen-by-sshd service-seen-by-sshd)]
                      [else (let-values ([(whocares) (place-channel-put izunac (list sshd-host 'notify "connecting"))]
                                         [(in out) (tcp-connect/enable-break sshd-host service-seen-by-sshd)])
                              (place-channel-put izunac (cons sshd-host (make-list 20 "00")))
                              (values in out))]))
              (let recv-match-send-loop : Void ()
                (match (sync/timeout/enable-break maxinterval /dev/tcpin)
                  [(? false?) (let ([reason (place-channel-put/get izunac (cons sshd-host maxinterval))])
                                (unless (false? reason) (error 'foxpipe "has to collapse: ~a!" reason)))]
                  [(? input-port?) (match (read /dev/tcpin)
                                     [(? eof-object?) (error 'foxpipe "remote server disconnected!")]
                                     [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                (recv-match-send-loop))))
          (sync/timeout/enable-break (+ (cast (random) Positive-Real) 1.0) never-evt)
          (catch-send-clear-loop))))))
