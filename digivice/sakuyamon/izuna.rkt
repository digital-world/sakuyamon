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

  (define-type Racket-Place-Status (Vector Fixnum Fixnum Fixnum Natural Natural Natural Natural Natural Fixnum Fixnum Natural Natural))
  (define-type NCurseWindow<%> (Class (field [stdscr Window*] [statusbar Window*])
                                      [refresh (-> [#:with (-> Window* Any)] Any)]
                                      [resize (-> Natural Natural Positive-Integer Positive-Integer Any)]
                                      [set-status (-> [#:clear? Boolean] [#:color (Option Color-Pair)] [#:offset Natural] [#:width Positive-Integer] Any * Any)]))

  (define-type Monitor<%> (Class #:implements NCurseWindow<%>
                                 (init-field [scepter-host String])
                                 [set-figureprint! (-> (Listof String) Any)]
                                 [visualize (-> Symbol ksysinfo Any)]))

  (define-type Console<%> (Class #:implements NCurseWindow<%>
                                 [add-syslog (-> String Syslog Any)]))

  (require/typed/provide racket
                         [vector-set-performance-stats! (-> Racket-Place-Status (Option Thread) Void)])

  (require/typed/provide racket/date
                         [current-date (-> date)] ;;; should be date*
                         [date-display-format (Parameterof Symbol)]
                         [date->string (-> date Boolean String)])
  
  (define uptime/base : Fixnum (current-milliseconds))
  (define pr+gctime/base : Fixnum (current-process-milliseconds))
  
  ; /+-----------+----------------------------------------+\
  ;  | title% [this is a ripped off line]                 |
  ;  +-----------+----------------------------------------+
  ;  | host1     | host2...n                              |
  ;  |           |                                        |
  ;  |           |                                        |
  ;  +-----------+----------------------------------------|
  ;  | rsyslog%                                           |
  ;  |                                                    |
  ;  |                                                    |
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

  (define-symdict console : (Instance Console<%>))
  (define-strdict monitor : (Instance Monitor<%>))

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

  (define ~t : (-> Natural Natural String)
    (lambda [n w]
      (~r #:min-width w #:pad-string "0" n)))
  
  (define ~% : (-> Flonum [#:precision (U Integer (List '= Integer))] String)
    (lambda [% #:precision [prcs '(= 2)]]
      (~r (* 100.0 (max 0 %)) #:precision prcs)))

  (define ~uptime : (-> Natural String)
    (lambda [s]
      (let*-values ([(d s) (quotient/remainder s 86400)]
                    [(h s) (quotient/remainder s 3600)]
                    [(m s) (quotient/remainder s 60)])
        (format "~a+~a:~a:~a" d (~t h 2) (~t m 2) (~t s 2)))))

  (define ~size : (-> Nonnegative-Real Symbol [#:precision (U Integer (List '= Integer))] String)
    (lambda [size unit #:precision [prcs '(= 3)]]
      (define-type/enum units : Unit 'Bytes 'KB 'MB 'GB 'TB)
      (let try-next-unit : String ([s size] [us (cast (member unit units) (Listof Unit))])
        (cond [(and (symbol=? (car us) 'Bytes) (< s 1024.0)) (~n_w (cast s Nonnegative-Integer) "Byte")]
              [(or (< s 1024.0) (zero? (sub1 (length us)))) (format "~a~a" (~r s #:precision prcs) (car us))]
              [else (try-next-unit (/ s 1024.0) ((inst cdr Unit Unit) us))]))))
  
  (define ~geolocation : (-> String String * String)
    (lambda [ip . whocares]
      (match (what-is-my-address ip)
        [(geolocation continent country _ #false _ _) (format "~a[~a/~a]" ip continent country)]
        [(geolocation _ country _ city _ _) (format "~a[~a ~a]" ip country city)]
        [false/else/error ip])))

  (define window% : NCurseWindow<%>
    (class object% (super-new)
      (field [stdscr (cast (newwin 0 0 0 0) Window*)]
             [statusbar (cast (newwin 0 0 0 0) Window*)])

      (define/public (resize y x lines cols)
        (wresize stdscr (cast (sub1 lines) Positive-Integer) cols)
        (mvwin stdscr y x)
        (wresize statusbar 1 cols)
        (mvwin statusbar (sub1 (+ y lines)) x)
        (wbkgdset statusbar 'StatusLine))

      (define/public (refresh #:with [smart-refresh wnoutrefresh])
        (touchwin statusbar)
        (smart-refresh stdscr)
        (smart-refresh statusbar))

      (define/public (set-status #:clear? [clear? #false] #:color [color #false] #:offset [x 0] #:width [width (getmaxx statusbar)] . contents)
        (unless (false? clear?) (wclear statusbar))
        (unless (empty? contents) (mvwaddstr statusbar 0 x (~a ((inst apply Any String) ~a contents) #:width width)))
        (unless (false? color) (mvwchgat statusbar 0 x width 'StatusLineNC color)))))
  
  (define host% : Monitor<%>
    (class window% (super-new)
      (inherit-field stdscr statusbar)
      (inherit set-status)

      (init-field scepter-host)

      (define titlebar : Window* (cast (newwin 0 0 0 0) Window*))
      (define figureprint : (Boxof (Listof String)) (box null))
      
      (define queue-length-max : (Boxof Positive-Integer) (box 1))
      (define status-queue : (Boxof (Listof ksysinfo)) (box null))
      
      (define/public set-figureprint!
        (lambda [figure-print]
          (set-box! figureprint figure-print)
          (wclear statusbar)))
      
      (define/public visualize
        (lambda [systype sample]
          (wclear stdscr)
          (mvwaddstr titlebar 0 0 (~a scepter-host (~a #:align 'right #:width (max 0 (- (getmaxx titlebar) (string-length scepter-host)))
                                                       (format "~a: ~a" systype (~uptime (ksysinfo-uptime sample))))))
          (visualize-slotbar "DSK" (ksysinfo-fstotal sample) (ksysinfo-fsfree sample) 0.70 0.85)
          (visualize-slotbar "RAM" (ksysinfo-ramtotal sample) (ksysinfo-ramfree sample) 0.70 0.85)
          (visualize-slotbar "SWP" (ksysinfo-swaptotal sample) (ksysinfo-swapfree sample) 0.70 0.85)
          (visualize-infinitebar (list "NIO" "Received" "Sent") (list (ksysinfo-nic_received sample) (ksysinfo-nic_sent sample)) (list 'PmenuThumb 'WildMenu))))
      
      (define/override (resize y x lines cols)
        (super resize (add1 y) x (cast (sub1 lines) Positive-Integer) cols)
        (wresize titlebar 1 cols)
        (mvwin titlebar y x)
        (wbkgdset titlebar 'TabLineFill)
        (mvwaddstr titlebar 0 0 (~a scepter-host #:width (getmaxx titlebar))))

      (define/override (refresh #:with [smart-refresh wnoutrefresh])
        (touchwin titlebar)
        (smart-refresh titlebar)
        (super refresh #:with smart-refresh))

      (define/private (visualize-slotbar [tag : String] [total : Natural] [free : Natural] [threshold-low : Flonum] [threshold-high : Flonum]) : Any
        (define used% : Flonum (- 1.0 (with-handlers ([void (const 0.0)]) (/ free total))))
        (define label : String (format "~a/~a(~a%)"
                                       (~size (max 0 (- total free)) 'KB #:precision '(= 1))
                                       (~size total 'KB #:precision '(= 1))
                                       (~% used% #:precision 0)))
        (define bary : Natural (getcury stdscr))
        (define barx : Natural (add1 (string-length tag)))
        (define slotsize : Natural (max 0 (- (getmaxx stdscr) barx 1)))
        (define usedsize : Natural (cast (exact-round (* slotsize used%)) Natural))
        (define barsize : Natural (cast (min (- slotsize (string-length label)) usedsize) Natural))
        (wattrset stdscr 'StorageClass)
        (mvwaddstr stdscr bary 0 tag)
        (wattrset stdscr 'MatchParen)
        (mvwaddstr stdscr bary (cast (sub1 barx) Nonnegative-Integer)
                   (~a #\[ (make-string barsize #\*) (~a label #:width (max 0 (- slotsize barsize)) #:align 'right) #\]))
        (mvwchgat stdscr bary barx slotsize 'Label 'Label)
        (mvwchgat stdscr bary barx usedsize 'MoreMsg 'MoreMsg)
        (when (> used% threshold-low)
          (define lowsize : Natural (cast (exact-round (* slotsize threshold-low)) Natural))
          (mvwchgat stdscr bary (+ barx lowsize) (max (- usedsize lowsize) 0) 'WarningMsg 'WarningMsg)
          (when (> used% threshold-high)
            (define highsize : Natural (cast (exact-round (* slotsize threshold-high)) Natural))
            (mvwchgat stdscr bary (+ barx highsize) (max (- usedsize highsize) 0) 'ErrorMsg 'ErrorMsg)))
        (wmove stdscr (add1 bary) 0))

      (define/private (visualize-infinitebar [tags : (Listof String)] [data : (Listof Natural)] [colors : (Listof Symbol)]) : Any
        (define bary : Natural (getcury stdscr))
        (wattrset stdscr 'StorageClass)
        (mvwaddstr stdscr bary 0 (car tags))
        (wattrset stdscr 'MatchParen)
        (waddch stdscr #\[)
        (let render-next ([tag : (Listof String) (cdr tags)]
                          [src : (Listof Natural) data]
                          [clr : (Listof Symbol) colors]
                          [barsize : Natural (max 0 (- (getmaxx stdscr) (string-length (car tags)) 2))])
          (unless (null? src)
            (define vsize : Natural (cast (with-handlers ([void (const 0)]) (exact-round (* barsize (/ (first src) (foldl + 0 src))))) Natural))
            (define label : String (~a (car tag) #\: (~size (max 0 (car src)) 'KB #:precision '(= 2)) #:width vsize #:align 'center))
            (wattrset stdscr (car clr))
            (waddstr stdscr label)
            (render-next (cdr tag) (cdr src) (cdr clr) (max 0 (- barsize vsize)))))
        (wattrset stdscr 'MatchParen)
        (waddch stdscr #\])
        (wmove stdscr (add1 bary) 0))

      (define/private (visualize-history [tag/lengend : String] [total : Natural] [free : Natural] [threshold-low : Flonum] [threshold-high : Flonum]) : Any
        (void))))

  (define rsyslog% : Console<%>
    (class window% (super-new)
      (inherit-field stdscr statusbar)
      (inherit resize refresh set-status)
      
      (scrollok stdscr #true)
      
      (define contents : (Boxof (Listof (Pairof String Syslog))) (box null))
      (define fields : (Listof Symbol) (list 'timestamp 'method 'host 'uri 'client 'user-agent 'referer))
    
      (define/public add-syslog : (-> String Syslog Any)
        (lambda [scepter-host record]
          (set-box! contents (cons (cons scepter-host record) (unbox contents)))
          (match record
            [(syslog5424 _ _ timestamp host app pid #false) (rich-status 'Ignore "~a@~a: ~a[~a]: [Empty Message]" timestamp host app pid)]
            [(syslog5424 _ _ timestamp host "taskgated" _ unsigned-signature) (rich-status 'Folded "~a@~a: ~a" timestamp host unsigned-signature)]
            [(syslog5424 _ _ timestamp host _ _ (? log:request? req)) (display-request scepter-host req)]
            [standard-rsyslog (display-rsyslog scepter-host standard-rsyslog)])))

      (define/private (display-rsyslog [scepter-host : String] [record : Syslog]) : Any
        (wattrset stdscr (hash-ref (color-links) (syslog5424-severity record) (const (list 0))))
        (waddwstr stdscr (~a #:max-width (getmaxx stdscr)
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
                                                        [,(string #\tab) "    "]))))))
      
      (define/private (display-request [scepter-host : String] [req : Log:Request]) : Any
        (define hilinks (color-links))
        (unless (empty? (cdr (unbox contents))) (waddch stdscr #\newline))
        (for ([field (in-list fields)])
          (wattrset stdscr (hash-ref hilinks field (const (list 0))))
          (waddstr stdscr (~a (request-ref req field)))
          (wstandend stdscr)
          (waddch stdscr #\space)))
      
      (define/private (request-ref [req : Log:Request] [key : Symbol]) : (Option String)
        (case key
          [(timestamp) (log:request-timestamp req)]
          [(client) (~geolocation (log:request-client req))]
          [else (hash-ref (log:request-headers req) key (const #false))]))

      (define/private (rich-status [colorpair : Color-Pair] [fmt : String] . [argl : Any *]) : Any
        (define message : String (apply format fmt argl))
        (set-status #:clear? #true #:color colorpair #:width (max (string-length message) 1) message))))

  (define display-statistics : (-> [#:stdbar Window*] [#:prefix String] Any)
    (lambda [#:stdbar [stdbar (:prefabwin 'titlebar)] #:prefix [prefix izuna-title]]
      (match-define (vector pr+gctime/now uptime/now gctime/now gctimes _ _ _ _ _ _ sysmem _)
        (and (vector-set-performance-stats! izuna-statistics #false) izuna-statistics))
      (define uptime : Positive-Fixnum (cast (- uptime/now uptime/base) Positive-Fixnum))
      (define pr+gctime : Positive-Fixnum (cast (- pr+gctime/now pr+gctime/base) Positive-Fixnum))
      (define status : String (~a #:align 'right #:width (max 0 (- (getmaxx stdbar) (string-length prefix)))
                                  (format "~a up, ~ams gc[~a], ~a% idle, ~a, ~a"
                                          (~uptime (quotient uptime 1000)) gctime/now gctimes
                                          (~% (- 1.0 (/ pr+gctime uptime)))
                                          (~size (+ (current-memory-use) sysmem) 'Bytes)
                                          (parameterize ([date-display-format 'iso-8601])
                                            (date->string (current-date) #true)))))
      (mvwaddstr stdbar 0 0 (string-append prefix status))
      (wrefresh stdbar)))
  
  (define alert : (-> Symbol String [#:stdbar Window*] Any * Any)
    (lambda [#:stdbar [stdbar (:prefabwin 'cmdlinebar)] higroup fmt . contents]
      (wclear stdbar)
      (wattrset stdbar higroup)
      (mvwaddstr stdbar 0 0 (~a (regexp-replaces (apply format fmt contents)
                                                 (list (list #px"\\s+" " ")
                                                       (list (path->string (digimon-zone)) "")))))
      (wrefresh stdbar)))
  
  (define update-windows-on-screen : (-> (Listof String) Any)
    (lambda [hosts]
      (define mtime : Natural (file-or-directory-modify-seconds (sakuyamon-colors.vim)))
      (when (< (unbox mtime-colors.vim) mtime)
        (:colorscheme! (sakuyamon-colors.vim))
        (set-box! mtime-colors.vim mtime)
        (on-digivice-resized hosts))
      (for ([stdwin (in-list (append ($monitor*) ($console*)))])
        (send (cast stdwin (Instance NCurseWindow<%>)) refresh #:with wnoutrefresh))
      (doupdate)))

  (define on-timer/second : (-> Natural Any)
    (lambda [times]
      (display-statistics)))

  (define on-digivice-resized : (-> (Listof String) Any)
    (lambda [hosts]
      (define columns : Positive-Integer (getmaxx (:prefabwin 'stdscr)))
      (define rows : Positive-Integer (getmaxy (:prefabwin 'stdscr)))
      (define host-rows : Positive-Integer (cast (exact-round (* rows 0.48)) Positive-Integer))
      (define rsyslog-rows : Positive-Integer (cast (- rows host-rows) Positive-Integer))
      (for-each wclear (list (:prefabwin 'stdscr) (:prefabwin 'cmdlinebar)))
      (wattrset (:prefabwin 'titlebar) 'Title)
      (display-statistics #:stdbar (:prefabwin 'titlebar) #:prefix izuna-title)
      (let resize-hosts ([winhosts : (Listof String) hosts]
                         [x : Nonnegative-Integer 0]
                         [cols : Natural columns])
        (unless (null? winhosts)
          (define monitor : (Instance Monitor<%>) ($monitor+ (car winhosts) (make-object host% (car winhosts))))
          (define host-cols : Positive-Integer (cast (exact-floor (/ cols (length winhosts))) Positive-Integer))
          (send monitor resize 0 x host-rows host-cols)
          (resize-hosts (cdr winhosts) (+ x host-cols) (cast (- cols host-cols) Natural))))
      (define console : (Instance Console<%>) ($console+ 'rsyslog (make-object rsyslog%)))
      (send console resize host-rows 0 rsyslog-rows columns)
      (for ([stdwin (in-list (append ($monitor*) ($console*)))])
        (send (cast stdwin (Instance NCurseWindow<%>)) set-status (object-name stdwin))
        (send (cast stdwin (Instance NCurseWindow<%>)) refresh #:with wnoutrefresh))
      (doupdate)))
  
  (define digivice-recv-match-render-loop : (-> Place (Listof String) Any)
    (lambda [foxpipe hosts]
      (with-handlers ([exn:fail? (lambda [[e : exn]] (alert 'ErrorMsg "~a" (exn-message e)))])
        (match (or (getch) (sync/timeout/enable-break 0.26149 #| Meissel–Mertens Constant |# foxpipe))
          [(? false? on-system-idle) (update-windows-on-screen hosts)]
          [(cons (? string? host) (vector (? string? message))) (send ($console 'rsyslog) add-syslog host (string->syslog message))]
          [(cons (? string? host) (vector (cons (? symbol? systype) (? ksysinfo? sample)))) (send ($monitor host) visualize systype sample)]
          [(cons (? string? host) (vector (cons (? symbol? errname) (? string? errmsg)))) (send ($monitor host) set-status errmsg #:color 'ErrorMsg)]
          [(cons (? string? host) (? flonum? s)) (place-channel-put foxpipe (format "idled ~as" s)) #| put/get is OK, no thread is (sync)ing |#]
          [(list (? string? host) (? string? figureprint) ...) (send ($monitor host) set-figureprint! (cast figureprint (Listof String)))]
          [(list (? string? host) 'notify (? string? fmt) argl ...) (send ($monitor host) set-status (apply format fmt argl))]
          [(list (? string? host) 'fail (? string? fmt) argl ...) (send ($monitor host) set-status (apply format fmt argl) #:color 'WarningMsg)]
          [(? char? c) (alert 'Ignore "Key pressed: ~s[~a]" c (char->integer c))]
          [(and (or 'SIGQUIT 'SIGINT) signal) (raise-signal-error signal)]
          [(or 'SIGWINCH) (on-digivice-resized hosts)]))
      (digivice-recv-match-render-loop foxpipe hosts)))
  
  (parameterize ([current-directory (find-system-path 'orig-dir)])
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
       (call-as-normal-termination
        #:atinit (thunk (ripoffline +1 'titlebar)
                        (ripoffline -1 'cmdlinebar)
                        (initscr)
                       
                        (unless (and (:prefabwin 'titlebar) (:prefabwin 'cmdlinebar) (:prefabwin 'stdscr)
                                     (curs_set 0) (raw) (noecho) (timeout 0) (intrflush #true) (keypad #true))
                          (error "NCurses is unavailable!"))
                       
                        (when (has_colors)
                          (start_color)
                          (use_default_colors)))
        #:atexit (thunk (endwin))
        (thunk (define hosts : (Listof String) (cons hostname other-hosts))
               (define $? : (Parameterof Integer) (make-parameter 130 #| SIGINT + 128|#))
               (update-windows-on-screen hosts) ; will load the up-to-date colorscheme
               (define timer : Thread (timer-thread 1.0 on-timer/second))
               (define foxpipe : Place (dynamic-place (cast `(submod ,(#%file) foxpipe) Module-Path) 'realize))
               (place-channel-put foxpipe hosts)
               (place-channel-put foxpipe (sakuyamon-scepter-port))
               (with-handlers ([exn:break? void]) ;;; All Signals are escaped to allow user killing blocked places
                 (digivice-recv-match-render-loop foxpipe hosts))
               
               (noraw) ;;; ncurses will not stop user signals
               (alert 'MoreMsg "Terminating Foxpipes")
               (place-break foxpipe 'terminate)
               (with-handlers ([exn:break? void])
                 (let wait-foxpipe ([rest-after-this-pass (sub1 (length (cons hostname other-hosts)))])
                   (match (sync/enable-break foxpipe)
                     [(? string? who) (alert 'Comment "Foxpipe@~a has collapsed." who)]
                     [_ (raise-signal-error 'SIGTERM)])
                   (if (zero? rest-after-this-pass)
                       ($? (place-wait foxpipe))
                       (wait-foxpipe (sub1 rest-after-this-pass)))))
               (place-kill foxpipe)
               (break-thread timer)
               (exit ($?)))))
     (list "hostname" "2nd hostname")
     (lambda [--help]
       (display (string-replace --help #px"  -- : .+?-h --'\\s*" ""))
       (exit 0)))))

#|===========================================================================================================================|#

(module* foxpipe typed/racket
  (provide (all-defined-out))
  
  (require (submod "../../digitama/posix.rkt" typed/ffi))
  (require (submod "../../digitama/foxpipe.rkt" typed/ffi))

  (require "../../digitama/digicore.rkt")
  
  (require/typed file/sha1
                 [bytes->hex-string (-> Bytes String)])

  (define kudagitsune : (-> Place-Channel Any)
    (lambda [izunac]
      (define/extract-symtable (thread-receive)
        [sshd-host : String]
        [host-seen-by-sshd : String]
        [service-seen-by-sshd : Index]
        [plaintransport? : Boolean]
        [username : String = (current-tamer)]
        [passphrase : String = ""]
        [rsa.pub : Path-String = (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")]
        [id_rsa : Path-String = (build-path (find-system-path 'home-dir) ".ssh" "id_rsa")])

      (define ssh-session : (Parameterof Foxpipe-Session*/Null) (make-parameter #false))

      (let/ec thread-exit : Symbol
        (let catch-send-clear-loop : Void ()
          (parameterize ([current-custodian (make-custodian)])
            (define (on-collapsed [signal : exn]) : Void
              (define session : Foxpipe-Session*/Null (ssh-session))
              (when (exn:fail? signal)
                (place-channel-put izunac (list sshd-host 'fail (exn-message signal))))
              (custodian-shutdown-all (current-custodian))
              (when (foxpipe-session*? session)
                (foxpipe_collapse session (~a signal)))
              (when (exn:break? signal)
                (thread-exit 'nothing)))
            
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
              (define maxinterval : Nonnegative-Real (+ (sakuyamon-foxpipe-sampling-interval) (/ (+ cputime gctime wallclock) 1000.0)))
              (define-values (/dev/tcpin /dev/tcpout)
                (cond [(foxpipe-session*? session) (foxpipe_direct_channel session host-seen-by-sshd service-seen-by-sshd)]
                      [else (let-values ([(whocares) (place-channel-put izunac (list sshd-host 'notify "connecting"))]
                                         [(in out) (tcp-connect/enable-break sshd-host service-seen-by-sshd)])
                              (place-channel-put izunac (cons sshd-host (make-list 20 "00")))
                              (values in out))]))
              (let recv-match-send-loop : Void ()
                (match (sync/timeout/enable-break maxinterval /dev/tcpin)
                  [(? false?) (let ([reason (place-channel-put/get izunac (cons sshd-host maxinterval))]) ;;; no thread is (sync)ing the place channel,
                                (unless (false? reason) (error 'foxpipe "has to collapse: ~a!" reason)))] ;;; so put/get will work as expected.
                  [(? input-port?) (match (with-handlers ([exn? values]) (read /dev/tcpin))
                                     [(? eof-object?) (error 'foxpipe "remote server disconnected!")]
                                     [(? exn? e) (place-channel-put izunac (cons sshd-host (vector (cons (object-name e) (exn-message e)))))]
                                     [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                (recv-match-send-loop))))
          (sync/timeout/enable-break (+ (cast (random) Positive-Real) 1.0) never-evt)
          (catch-send-clear-loop)))))
  
  (define realize : Place-Main
    (lambda [izunac]
      (define-strdict foxpipe : Thread)
      
      (define scepter-hosts : (Listof String) (cast (place-channel-get izunac) (Listof String)))
      (define scepter-port : Index (cast (place-channel-get izunac) Index))

      (libssh2_init)
      (for ([scepter-host : String (in-list scepter-hosts)])
        (thread-send ($foxpipe+ scepter-host (thread (thunk (kudagitsune izunac))))
                     ((inst hasheq Symbol Any)
                      'sshd-host scepter-host
                      'host-seen-by-sshd "localhost"
                      'service-seen-by-sshd scepter-port
                      'plaintransport? (and (or (member scepter-host '("localhost" "::1"))
                                                (regexp-match? #px"^127\\." scepter-host)) #true))))
      (with-handlers ([exn:break? void])
        (sync/enable-break never-evt))
      (for-each break-thread ($foxpipe*))
      (let wait-foxpipe ()
        (define who : String
          (apply sync/enable-break
                 (hash-map %foxpipe (lambda [[host : String] [this : Thread]] ((inst wrap-evt Any String) (thread-dead-evt this) (const host))))))
          (place-channel-put izunac who)
          ($foxpipe- who)
        (unless (zero? ($foxpipe#))
          (wait-foxpipe)))
      (libssh2_exit))))
