#lang typed/racket

(provide (all-defined-out))

(define desc : String "Monitor rsyslogs via SSH Tunnel")

(module* syslog-rfc5424 racket
  (provide (all-defined-out))

  ;;; TODO: This module should be implemented in Typed Racket
  ;;; but meanwhile structs in this module has lots of uncovertable contracts.

  (require "../../digitama/posix.rkt")
  (require "../../digitama/geolocation.rkt")

  (struct log:message () #:prefab)
  (struct log:request log:message (timestamp method uri client host user-agent referer headers) #:prefab)
  (struct syslog (facility severity timestamp loghost sender #| also known as TAG |# pid message) #:prefab)

  (define string->syslog
    (lambda [log-text]
      (define template (pregexp (format "^<(~a)>\\s*(~a)\\s+(~a)\\s+(~a)(~a)?:?\\s*(~a)?\\s*(~a)?$"
                                        "\\d{1,3}" #| prival |#
                                        "[^:]+[^ ]+" #| timestamp |#
                                        "[^ ]+" #| hostname |#
                                        "[^[]+" #| appname |#
                                        "\\[\\d+\\]"  #| procid |#
                                        "\\[[^]]+\\]" #| structured data, just ignored |#
                                        ".+" #| free-from message |#)))
      (define (~geolocation ip . whocares)
        (define geo (what-is-my-address ip))
        (cond [(false? geo) ip]
              [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
              [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))]))
      (match (regexp-match template log-text)
        [(? false?) (error 'string->syslog "Invalid Syslog Message: ~a" log-text)]
        [(list _ prival timestamp hostname appname procid _ ffmsg)
         (let-values ([[facility severity] (quotient/remainder (string->number prival) 8)])
           (syslog ((ctype-c->scheme _facility) (arithmetic-shift facility 3))
                   ((ctype-c->scheme _severity) severity)
                   timestamp
                   hostname
                   appname
                   (and (string? procid) (string->number (string-trim procid #px"[[\\]]")))
                   (match ffmsg
                     [(? false?) #false]
                     [(pregexp #px"\\s*request:\\s*(.+)" (list _ hstr)) (string->request hstr)]
                     [else (regexp-replace* #px"\\d{1,3}(\\.\\d{1,3}){3}" ffmsg ~geolocation)])))])))
  
  (define (string->request hstr)
    (define headers (for/hash ([(key val) (in-hash (read (open-input-string hstr)))])
                      (values key (if (bytes? val) (bytes->string/utf-8 val) val))))
    (log:request (hash-ref headers 'logging-timestamp (const #false))
                 (hash-ref headers 'method (const #false))
                 (hash-ref headers 'uri (const #false))
                 (hash-ref headers 'client (const #false))
                 (hash-ref headers 'host (const #false))
                 (hash-ref headers 'user-agent (const #false))
                 (hash-ref headers 'referer (const #false))
                 headers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meanwhile Typed Racket does not support CPonter well, So leave the "Typed C" to FFI itself. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* sakuyamon racket
  (require syntax/location)

  (require racket/date)
  
  (require (submod ".."))
  (require (submod ".." syslog-rfc5424))
  (require "../../digitama/termctl.rkt")

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
  ;  | commandline%, this is a ripped off line.           |
  ; \+----------------------------------------------------+/
  
  (define sakuyamon-scepter-port (make-parameter (sakuyamon-foxpipe-port)))
  (define sakuyamon-colors.vim (make-parameter (build-path (digimon-stone) "colors.vim")))
  (define sakuyamon-action (path-replace-suffix (file-name-from-path (quote-source-file)) #""))

  (define uptime/base (current-milliseconds))
  (define pr+gctime/base (current-process-milliseconds))
  (define izuna-statistics (make-vector 11))
  (define mtime-colors.vim (box -inf.0))

  (define color-links
    (let* ([colors.rktl (build-path (digimon-stone) "colors.rktl")]
           [mtime (box -inf.0)]
           [links (box (make-hash))])
      (lambda []
        (define last-mtime (file-or-directory-modify-seconds colors.rktl #false (const #false)))
        (when (and (integer? last-mtime) (< (unbox mtime) last-mtime))
          (set-box! mtime last-mtime)
          (set-box! links (with-handlers ([exn? (lambda [e] (unbox links))])
                            (eval (with-input-from-file colors.rktl read)))))
        (unbox links))))
  
  (define titlebar (make-parameter #false))
  (define cmdlinebar (make-parameter #false))
  (define stdhost (make-parameter #false))
  (define stdreq (make-parameter #false))
  (define stdlog (make-parameter #false))

  (define window%
    (class object% (super-new)
      (field [monitor (newwin 0 0 0 0)])
      (field [statusbar (newwin 1 0 0 0)])
      
      (define/public (set-status #:update? [update? #true] #:clear? [clear? #false] #:color [color #false] #:offset [x 0] #:width [width (getmaxx statusbar)] . contents)
        (unless (false? clear?) (wclear statusbar))
        (unless (empty? contents) (mvwaddwstr statusbar 0 x (apply ~a contents #:width width)))
        (unless (false? color) (mvwchgat statusbar 0 x width 'StatusLineNC color))
        (when update? (wrefresh statusbar)))
      
      (define/public (resize y x lines cols)
        (wresize monitor (sub1 lines) cols)
        (mvwin monitor y x)
        (wresize statusbar 1 cols)
        (mvwin statusbar (sub1 (+ y lines)) x)
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
        (set-status sceptor-host)
        (refresh #:update? #true))

      (define/public (beat-heart scepter-host)
        (set-status scepter-host #:update? #false)
        (set-status #:offset (random (string-length scepter-host)) #:width 1 #:color 'SpecialChar))
      
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
        (define hilinks (color-links))
        (unless (empty? (cdr (unbox contents))) (waddch monitor #\newline))
        (for ([field (in-list fields)])
          (wattrset monitor (hash-ref hilinks field (const (list 0))))
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
        (wattrset monitor (hash-ref (color-links) (syslog-severity record) (const (list 0))))
        (waddwstr monitor (~syslog scepter-host record))
        (refresh #:update? #true))

      (define/private (~syslog scepter-host record)
        (~a #:max-width (getmaxx monitor)
            (format "~a~a ~a ~a ~a[~a]: ~a"
                    (if (empty? (cdr (unbox contents))) "" #\newline)
                    (~a (syslog-facility record) #:width 8)
                    (syslog-timestamp record)
                    scepter-host
                    (syslog-sender record)
                    (syslog-pid record)
                    (regexp-replaces (~a (syslog-message record))
                                     `([,(string #\newline) " "]
                                       [,(string #\tab) "    "])))))))

  (define rich-echo
    (lambda [higroup #:stdwin [stdwin (cmdlinebar)] fmt . contents]
      (wclear stdwin)
      (wattrset stdwin higroup)
      (mvwaddstr stdwin 0 0 (apply format fmt contents))
      (wrefresh stdwin)))

  (define on-timer/second
    (lambda [times]
      (match-define (vector pr+gctime/now uptime/now gctime/now gctimes _ _ _ _ _ sysmem _)
        (and (vector-set-performance-stats! izuna-statistics #false) izuna-statistics))
      (let*-values ([(uptime pr+gctime) (values (- uptime/now uptime/base) (- pr+gctime/now pr+gctime/base))]
                    [(s) (quotient/remainder uptime 1000)]
                    [(d s) (quotient/remainder s 86400)]
                    [(h s) (quotient/remainder s 3600)]
                    [(m s) (quotient/remainder s 60)]
                    [(~t) (lambda [n w] (~r #:min-width w #:pad-string "0" n))]
                    [(~m) (lambda [m] (~r #:precision '(= 3) (/ m 1024.0 1024.0)))])
        (define status (format "uptime ~a ~a:~a:~a, ~ams gc[~a], ~a% idle, ~aMB, ~a"
                               (~n_w d "day") (~t h 2) (~t m 2) (~t s 2) gctime/now gctimes
                               (~r #:precision '(= 2) (* 100.0 (max 0 (- 1.0 (/ pr+gctime uptime)))))
                               (~m (+ (current-memory-use) sysmem))
                               (parameterize ([date-display-format 'iso-8601])
                                 (date->string (current-date) #true))))
        (define width (+ (string-length status) 8))
        (mvwaddstr (titlebar) 0 (- (getmaxx (titlebar)) width) (~a status #:align 'right #:width width))
        (wrefresh (titlebar)))))

  (define on-digivice-resized
    (lambda []
      (define-values [columns rows] (values (c-extern 'COLS _int) (c-extern 'LINES _int)))
      (define-values [host-cols rsyslog-rows] (values (exact-round (* columns 0.16)) (exact-round (* rows 0.24))))
      (define-values [request-cols request-rows] (values (- columns host-cols 1) (- rows rsyslog-rows)))
      (for-each (lambda [stdwin] (and (wclear (stdwin)) (wnoutrefresh (stdwin)))) (list stdscr titlebar cmdlinebar))
      (wattrset (titlebar) 'Title)
      (mvwaddstr (titlebar) 0 0 (~a (current-digimon) #\space sakuyamon-action #\[ (getpid) #\] #:width columns))
      (on-timer/second 0)
      (for ([win% (in-list (list host%       request%         rsyslog%))]
            [winp (in-list (list stdhost     stdreq           stdlog))]
            [scry (in-list (list 0           0                request-rows))]
            [scrx (in-list (list 0           (add1 host-cols) (add1 host-cols)))]
            [scrr (in-list (list rows        request-rows     rsyslog-rows))]
            [scrc (in-list (list host-cols   request-cols     request-cols))])
        (unless (winp) (winp (make-object win%)))
        (send (winp) resize scry scrx scrr scrc))
      (attrset 'VertSplit)
      (mvvline 0 host-cols (- rows 1))
      (mvaddch (sub1 rows) host-cols (acs_map 'DARROW #:extra-attrs (list 'underline)))
      (standend)
      (for-each wnoutrefresh (list (stdscr) (titlebar)))
      (doupdate)))
  
  (define on-foxpipe-rsyslog
    (lambda [scepter-host record]
      (define rich-status (lambda [clr msg] (send (stdlog) set-status #:clear? #true #:color clr #:width (string-length msg) msg)))
      (match record
        [(syslog _ _ timestamp host app pid #false) (rich-status 'Ignore (format "~a@~a: ~a[~a]: [Empty Message]" timestamp host app pid))]
        [(syslog _ _ timestamp host "taskgated" _ unsigned-signature) (rich-status 'Folded (format "~a@~a: ~a" timestamp host unsigned-signature))]
        [(syslog _ _ _ _ _ _ (struct log:request _)) (send (stdreq) add-record! scepter-host record)]
        [(struct syslog _) (send (stdlog) add-record! scepter-host record)])))
  
  (define on-system-idle
    (lambda []
      (let ([mtime (file-or-directory-modify-seconds (sakuyamon-colors.vim))])
        (when (< (unbox mtime-colors.vim) mtime)
          (:colorscheme! (sakuyamon-colors.vim))
          (set-box! mtime-colors.vim mtime)
          (on-digivice-resized)))))
  
  (define digivice-izuna-monitor-main
    (lambda [timer foxpipes]
      (define (on-signal signal)
        (rich-echo 'MoreMsg "Terminating Foxpipes")
        (for-each (lambda [foxpipe] (place-break foxpipe 'terminate)) (hash-values foxpipes))
        (let wait-foxpipe ([fps foxpipes])
          (unless (zero? (hash-count fps))
            (define who (apply sync (hash-map fps (lambda [host foxpipe] (wrap-evt (place-dead-evt foxpipe) (const host))))))
            (rich-echo 'Comment "Foxpipe@~a has collapsed." who)
            (wait-foxpipe (hash-remove fps who))))
        (break-thread timer 'terminate))
      (with-handlers ([exn:break? on-signal]) ;;; System Signal also be caught here
        (let recv-match-render-loop () #| TODO: Mac's heart beat might be received as a strange char |#
          (with-handlers ([exn:fail? (lambda [e] (rich-echo 'ErrorMsg "~a" (string-join (string-split (exn-message e)))))])
            (match (or (getch) (apply sync/timeout/enable-break 0.26149 #| Meissel–Mertens Constant |# (hash-values foxpipes)))
              [(? false?) (on-system-idle)]
              [(cons host (vector (? char? heart-beat))) (send (stdhost) beat-heart host)]
              [(cons host (vector (? string? message))) (on-foxpipe-rsyslog host (string->syslog message))]
              [(cons host (vector message)) (rich-echo (rich-echo 'WarningMsg "Received an unexpected message from ~a: ~s" host message))]
              [(cons host (? flonum? s)) (place-channel-put (hash-ref foxpipes host) (format "idled ~as" s))]
              [(list host (? string? figureprint) ...) (send (stdhost) add-host! host figureprint)]
              [(list host 'notify (? string? fmt) argl ...) (send (stdhost) set-status host ": " (apply format fmt argl))]
              [(list host 'fail (? string? fmt) argl ...) (rich-echo 'WarningMsg "~a: ~a" host (apply format fmt argl))]
              [(list host 'error (? string? fmt) argl ...) (error (format "~a: ~a" host ": " (apply format fmt argl)))]
              [(? char? c) (rich-echo 'Ignore "Key pressed: ~s[~a]" c (char->integer c))]
              [(or 'SIGQUIT) (let/ec collapse (raise (exn:break:terminate "user terminate" (current-continuation-marks) collapse)))]
              [(or 'SIGINT) (let/ec collapse (raise (exn:break "user break" (current-continuation-marks) collapse)))]
              [(or 'SIGWINCH) (on-digivice-resized)]))
          (recv-match-render-loop)))))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            (parse-command-line
             (format "~a ~a" (last (quote-module-name)) sakuyamon-action)
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [["-c"] ,(lambda [flag cs.vim] (when (file-exists? cs.vim) (sakuyamon-colors.vim cs.vim)))
                        ["Use an alternative color scheme <colors.vim>." "colors.vim"]]
                [["-p"] ,(lambda [flag port] (sakuyamon-scepter-port (string->number port)))
                        ["Use an alternative service <port>." "port"]]])
             (lambda [!flag hostname . other-hosts]
               (dynamic-wind (thunk (and (ripoffline +1 titlebar)
                                         (ripoffline -1 cmdlinebar)
                                         (initscr)))
                             (thunk (with-handlers ([exn:fail? (lambda [e] (and (endwin) (displayln (exn-message e))))])
                                      (unless (and (titlebar) (cmdlinebar) (stdscr) (curs_set 0)
                                                   (raw) (noecho) (timeout 0) (intrflush #true) (keypad #true))
                                        (error "NCurses is unavailable!"))
                                      (when (has_colors)
                                        (start_color)
                                        (use_default_colors))
                                      (on-system-idle) ; will load the up-to-date colorscheme
                                      ((curry digivice-izuna-monitor-main)
                                       (thread (thunk (for ([t (in-naturals 1)])
                                                        ; -0.001 adjustment works fine
                                                        (sync/timeout/enable-break 0.999 never-evt)
                                                        (on-timer/second t))))
                                       (for/hash ([scepter-host (in-list (cons hostname other-hosts))])
                                         (define foxpipe (dynamic-place `(submod (file ,(quote-source-file)) foxpipe) 'realize))
                                         ((curry place-channel-put foxpipe)
                                          (hasheq 'sshd-host scepter-host
                                                  'host-seen-by-sshd "localhost"
                                                  'service-seen-by-sshd (sakuyamon-scepter-port)
                                                  'plaintransport? (not (not (or (member scepter-host '("localhost" "::1"))
                                                                                 (regexp-match? #px"^127\\." scepter-host))))))
                                         (values scepter-host foxpipe)))))
                             (thunk (endwin))))
             (list "hostname" "2nd hostname")
             (lambda [--help]
               (display (string-replace --help #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))

#|===============================================================================================|#

(module* foxpipe racket
  (provide (all-defined-out))
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/foxpipe.rkt")
  
  (require file/sha1)

  (define the-one-manages-foxpipe-or-plaintcp-io (make-parameter (make-custodian)))
  (define ssh-session (make-parameter #false))
  
  (define (on-collapsed sshd-host izunac reason #:libssh2_exit? [exit? #false])
    (with-handlers ([exn? (compose1 (curry place-channel-put izunac) (curry list sshd-host 'error) exn-message)])
      ;;; shutdown channels and plaintcp ports
      (custodian-shutdown-all (the-one-manages-foxpipe-or-plaintcp-io))
      (the-one-manages-foxpipe-or-plaintcp-io (make-custodian))
      (when (ssh-session)
        ;;; shutdown ssh tcp ports
        (foxpipe_collapse (ssh-session) (~a reason))
        (unless (false? exit?)
          (libssh2_exit)))))
  
  (define realize
    (lambda [izunac]
      (with-handlers ([exn:break? void])
        (define argh (place-channel-get izunac))
        (match-define (list sshd-host host-seen-by-sshd service-seen-by-sshd plaintransport?)
          (map (curry hash-ref argh) '(sshd-host host-seen-by-sshd service-seen-by-sshd plaintransport?)))
        (match-define (list username passphrase rsa.pub id_rsa)
          (map (curry hash-ref argh)
               '(username passphrase rsa.pub id_rsa)
               (list (current-tamer) ""
                     (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")
                     (build-path (find-system-path 'home-dir) ".ssh" "id_rsa"))))

        (with-handlers ([exn:break? (curry on-collapsed sshd-host izunac #:libssh2_exit? #true)])
          (let catch-send-clear-loop ()
            (define (on-signal/sendback signal)
              (place-channel-put izunac (list sshd-host 'fail (exn-message signal)))
              (on-collapsed sshd-host izunac signal))
            (with-handlers ([exn:fail? on-signal/sendback])
              (match-define-values (_ cputime wallclock gctime)
                ((curryr time-apply null)
                 (lambda []
                   (collect-garbage)
                   (when (false? plaintransport?)
                     (place-channel-put izunac (list sshd-host 'notify "constructing ssh channel"))
                     (ssh-session (foxpipe_construct sshd-host 22))
                     (define figureprint (foxpipe_handshake (ssh-session) 'LIBSSH2_HOSTKEY_HASH_SHA1))
                     (place-channel-put izunac (cons sshd-host (regexp-match* #px".." (string-upcase (bytes->hex-string figureprint)))))
                     (foxpipe_authenticate (ssh-session) username rsa.pub id_rsa passphrase)))))
              (parameterize ([current-custodian (the-one-manages-foxpipe-or-plaintcp-io)])
                (define maxinterval (+ (sakuyamon-foxpipe-idle) (/ (+ cputime gctime) 1000.0)))
                (define-values (/dev/tcpin /dev/tcpout)
                  (cond [(ssh-session) (foxpipe_direct_channel (ssh-session) host-seen-by-sshd service-seen-by-sshd)]
                        [else (and (place-channel-put izunac (list sshd-host 'notify "connecting"))
                                   (let-values ([(in out) (tcp-connect/enable-break sshd-host service-seen-by-sshd)])
                                     (place-channel-put izunac (cons sshd-host (make-list 20 "00")))
                                     (values in out)))]))
                (let recv-match-send-loop ()
                  (match (sync/timeout/enable-break maxinterval /dev/tcpin)
                    [(? false?) (let ([reason (place-channel-put/get izunac (cons sshd-host maxinterval))])
                                  (unless (false? reason) (error 'foxpipe "has to collapse: ~a!" reason)))]
                    [(? input-port?) (match (read /dev/tcpin)
                                       [(? eof-object?) (error 'foxpipe "remote server disconnected!")]
                                       [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                  (recv-match-send-loop))))
            (sync/timeout/enable-break (+ (random) 1.0) never-evt)
            (catch-send-clear-loop)))))))
