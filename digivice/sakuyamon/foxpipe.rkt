#lang typed/racket

(provide (all-defined-out))

(define desc : String "Launch the rsyslog proxy server")

(module+ sakuyamon
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/daemon.rkt")

  (require/typed racket/base
                 [exn:break:hang-up? (-> Any Boolean)]
                 [exn:break:terminate? (-> Any Boolean)])

  (define max-size : Natural 65535)
  (define log-pool : Bytes (make-bytes max-size))
  (define foxpipe : UDP-Socket (udp-open-socket))
  (define scepter : (Parameterof (Option TCP-Listener)) (make-parameter #false))
  (define izunas : (HashTable Input-Port Output-Port) ((inst make-hash Input-Port Output-Port)))
  
  (define foxlog : (-> Symbol String Any * Void)
    (lambda [severity maybe . argl]
      (rsyslog (severity.c severity) 'foxpipe (apply format maybe argl))))

  (define exit-with-eperm : (-> Symbol Natural Nothing)
    (lambda [tips no]
      (foxlog 'error "~a error: ~a; errno=~a" tips (strerror no) no)
      (exit 'EPERM)))

  (define exit-with-fatal : (-> exn Nothing)
    (lambda [e]
      (foxlog 'error (exn-message e))
      (exit 'FATAL)))
  
  (define signal-handler : (-> exn Void)
    (lambda [signal]
      (unless (port-closed? (current-output-port)) (newline))
      (foxlog 'notice "terminated by ~a."
                     (cond [(exn:break:hang-up? signal) 'SIGHUP]
                           [(exn:break:terminate? signal) 'SIGTERM]
                           [else 'SIGINT]))
      (when (exn:break:hang-up? signal) (raise signal))))
  
  (define serve-forever : (-> (Evtof (List Natural String Natural)) TCP-Listener Void)
    (lambda [/dev/udp /dev/tcp]
      (with-handlers ([exn:break? signal-handler])
        (match (apply sync/timeout/enable-break 8.0 /dev/udp /dev/tcp ((inst hash-keys Input-Port Output-Port) izunas))
          [{list size _ _} (when (or (terminal-port? (current-output-port)) (positive? (hash-count izunas)))
                             (define packet (bytes->string/utf-8 log-pool #false 0 size))
                             (when (terminal-port? (current-output-port)) (displayln packet))
                             (for ([/dev/iznout ((inst in-hash-values Input-Port Output-Port) izunas)])
                               (displayln packet /dev/iznout)))]
          [{? tcp-listener?} (let-values ([{/dev/tcpin /dev/tcpout} (tcp-accept /dev/tcp)])
                               (s-exp->fasl "hello, young man!\n" /dev/tcpout)
                               (displayln (fasl->s-exp /dev/tcpin))
                               ((inst hash-set! Input-Port Output-Port) izunas /dev/tcpin /dev/tcpout))]
          [#false void])
        (serve-forever /dev/udp /dev/tcp))))

  ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any Void) (Listof String) (-> String Void) Void))
   (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
   (current-command-line-arguments)
   `{{usage-help ,(format "~a~n" desc)}}
   (lambda [[flags : Any]]
     (parameterize ([current-custodian (make-custodian)])
       (dynamic-wind (thunk (let ([port (or (sakuyamon-foxpipe-port) 514)])
                              (define root? (zero? (getuid)))
                              (unless (zero? (seteuid (getuid))) (exit-with-eperm 'regain-uid (saved-errno)))
                              (unless (zero? (setegid (getgid))) (exit-with-eperm 'regain-gid (saved-errno)))
                              (define-values {errno uid gid} (fetch_tamer_ids #"tamer"))
                              (when (and root? (not (zero? errno))) (exit-with-eperm 'fetch-tamer-id errno))
                              
                              (with-handlers ([exn:fail:network? exit-with-fatal])
                                (udp-bind! foxpipe "127.0.0.1" port)
                                (scepter (tcp-listen port)))
                              
                              (when root?
                                ;;; if change uid first, then gid cannot be changed again.
                                (unless (zero? (setegid gid)) (exit-with-eperm 'drop-gid (saved-errno)))
                                (unless (zero? (seteuid uid)) (exit-with-eperm 'drop-uid (saved-errno))))
                              
                              (when (zero? (geteuid))
                                (foxlog 'error "Misconfigured: Privilege Has Not Dropped!")
                                (exit 'ECONFIG))
                              
                              (match-let-values ([{_ port _ _} (udp-addresses foxpipe #true)])
                                (foxlog 'notice "waiting rsyslog packets on ~a." port))))
                     (thunk (serve-forever (udp-receive!-evt foxpipe log-pool)
                                           (cast (scepter) TCP-Listener)))
                     (thunk (custodian-shutdown-all (current-custodian))))))
   null
   (lambda [[-h : String]]
     (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
     (exit 0))))
