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

  (define sakuyamon-launch-time (current-seconds))
  (define mtime-colors.vim (box +inf.0))

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
      
      (define/public (set-status #:update? [update? #true] #:color-pair [color #false] #:offset [x 0] #:width [width (getmaxx statusbar)] . contents)
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
        (refresh #:update? #true))
      
      (define/public (show-system-status)
        (define-values (d h m s)
          (let*-values ([(uptime) (- (current-seconds) sakuyamon-launch-time)]
                        [(d uptime) (quotient/remainder uptime 86400)]
                        [(h uptime) (quotient/remainder uptime 3600)]
                        [(m uptime) (quotient/remainder uptime 60)])
            (values d h m uptime)))
        (set-status "up" #\space d "d" #\space h #\: m #\: s #\space
                    (~r #:precision '(= 3) (/ (current-memory-use) 1024.0 1024.0)) "MB"))
      
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
                    (string-replace (~a (syslog-message record)) (string #\newline) " "))))))

  (define rich-echo
    (lambda [higroup fmt . contents]
      (define stdwin (cmdlinebar))
      (wclear stdwin)
      (wattrset stdwin higroup)
      (mvwaddstr stdwin 0 0 (apply format fmt contents))
      (wrefresh stdwin)))
  
  (define on-digivice-resized
    (lambda []
      (define-values [columns rows] (values (c-extern 'COLS _int) (c-extern 'LINES _int)))
      (define-values [host-cols rsyslog-rows] (values (exact-round (* columns 0.16)) (exact-round (* rows 0.24))))
      (define-values [request-cols request-rows] (values (- columns host-cols 1) (- rows rsyslog-rows)))
      (for-each (lambda [stdwin] (and (wclear (stdwin)) (wnoutrefresh (stdwin)))) (list stdscr titlebar cmdlinebar))
      (wattrset (titlebar) 'Title)
      (mvwaddstr (titlebar) 0 0 (~a (current-digimon) #\space sakuyamon-action #\[ (getpid) #\] #:width columns))
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
      (match record
        [(syslog _ _ _ _ app pid #false) (rich-echo 'Ignore "Recieved an empty message from ~a[~a]@~a~n" app pid scepter-host)]
        [(syslog _ _ _ _ "taskgated" _ unsigned-signature) (rich-echo 'Folded "~a" unsigned-signature)]
        [(syslog _ _ _ _ _ _ (struct log:request _)) (send (stdreq) add-record! scepter-host record)]
        [(struct syslog _) (send (stdlog) add-record! scepter-host record)])))
  
  (define on-system-idle
    (lambda []
      (let ([mtime (file-or-directory-modify-seconds (sakuyamon-colors.vim) #false (const 0))])
        (when (< (unbox mtime-colors.vim) mtime)
          (:colorscheme! (sakuyamon-colors.vim))
          (set-box! mtime-colors.vim mtime)
          (on-digivice-resized)))
      (send (stdhost) show-system-status)))
  
  (define digivice-izuna-monitor-main
    (lambda [foxpipes]
      (define (on-signal signal)
        (rich-echo 'MoreMsg "Terminating Foxpipes")
        (for-each (lambda [foxpipe] (place-break foxpipe 'terminate)) (hash-values foxpipes))
        (let wait-foxpipe ([fps foxpipes])
          (unless (zero? (hash-count fps))
            (define who (apply sync (hash-map fps (lambda [host foxpipe] (wrap-evt (place-dead-evt foxpipe) (const host))))))
            (rich-echo 'Comment "Foxpipe@~a has collapsed." who)
            (wait-foxpipe (hash-remove fps who)))))
      (with-handlers ([exn:break? on-signal]) ;;; System Signal also be caught here
        (let recv-match-render-loop () #| TODO: Mac's heart beat might be received as a strange char |#
          (with-handlers ([exn:fail? (lambda [e] (rich-echo 'ErrorMsg "~a" (string-join (string-split (exn-message e)))))])
            (match (or (getch) (apply sync/timeout/enable-break 0.26149 #| Meissel–Mertens Constant |# (hash-values foxpipes)))
              [(? false?) (on-system-idle)]
              [(cons host (vector (? char? heart-beat))) (send (stdhost) set-status host)]
              [(cons host (vector (? string? message))) (on-foxpipe-rsyslog host (string->syslog message))]
              [(cons host (vector message)) (rich-echo (rich-echo 'WarningMsg "Recieved an unexpected message from ~a: ~s" host message))]
              [(cons host (? flonum? s)) (place-channel-put (hash-ref foxpipes host) (format "idled ~as" s))]
              [(list host (? string? figureprint) ...) (send (stdhost) add-host! host figureprint)]
              [(list host 'fail (? string? fmt) argl ...) (rich-echo 'WarningMsg "~a: ~a" host (apply format fmt argl))]
              [(list host 'notify (? string? fmt) argl ...) (send (stdhost) set-status host ": " (apply format fmt argl))]
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
                                        (use_default_colors)
                                        (:colorscheme! (sakuyamon-colors.vim))
                                        (set-box! mtime-colors.vim (file-or-directory-modify-seconds (sakuyamon-colors.vim))))
                                      (on-digivice-resized)
                                      ((curry digivice-izuna-monitor-main)
                                       (for/hash ([scepter-host (in-list (cons hostname other-hosts))])
                                         (define foxpipe (dynamic-place `(submod (file ,(quote-source-file)) foxpipe) 'realize))
                                         ((curry place-channel-put foxpipe)
                                          (hasheq 'sshd-host scepter-host
                                                  'host-seen-by-sshd "localhost"
                                                  'service-seen-by-sshd (sakuyamon-scepter-port)))
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
                  (foxpipe_collapse (ssh-session) reason))
                (collect-garbage))
              (cond [(exn:break:terminate? maybe-exn) (libssh2_exit)]
                    [else (catch-send-clear-loop (+ (sin (current-inexact-milliseconds)) 1.0))])))
          (with-handlers ([exn? terminate/sendback-if-failed])
            (sync/enable-break (alarm-evt (+ (current-inexact-milliseconds) (* (abs delay) 1000))))
            (place-channel-put izunac (list sshd-host 'notify "constructing ssh channel."))
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
                       (error 'foxpipe "foxpipe has to collapse: ~a!" reason)))]
                  [(? input-port?)
                   (match (read /dev/sshdin)
                     [(? eof-object?) (error 'foxpipe "remote server disconnected!")]
                     [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                (recv-match-send-loop)))))))))
