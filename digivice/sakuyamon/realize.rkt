#lang typed/racket

(provide (all-defined-out))

; TODO: this web server needs to be reimplemented on my own
(define desc : String "Launch the web server with listening all addresses")

(module+ sakuyamon
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/dispatch.rkt")
  (require "../../digitama/unstable-typed/web-server/dispatchers.rkt")
  (require "../../digitama/unstable-typed/web-server/private.rkt")
  
  (require (submod "../../digitama/posix.rkt" typed/ffi))

  (require/typed racket
                 [#:struct (exn:fail:network:errno exn:fail:network)
                           ([errno : (Pairof Natural (U 'posix 'windows 'gai))])]
                 [port-closed-evt (-> Port (Rec x (Evtof x)))]
                 [peek-bytes-evt (-> Natural Natural False Input-Port (Evtof (U Bytes EOF)))])

  (define syslog-perror : (-> Symbol String Any * Void)
    (lambda [severity maybe . argl]
      (define message (apply format maybe argl))
      (define topic 'realize)
      (with-handlers ([exn:fail? void]) ;;; maybe broken pipe or closed port
        (case severity
          [(notice) (and (printf "~a[~a]: ~a~n" topic (getpid) message) (flush-output))]
          [else (eprintf "~a[~a]: ~a~n" topic (getpid) message)]))
      (rsyslog severity topic message)))

  (define exit-with : (-> Symbol (-> Any Nothing))
    (lambda [reason]
      (lambda [desc]
        (syslog-perror 'error "~a" (if (exn? desc) (exn-message desc) desc))
        (exit reason))))

  (define signal-handler : (-> exn (U Void Nothing))
    (lambda [signal]
      (unless (port-closed? (current-output-port)) (newline))
      (define signame : Symbol
        (cond [(exn:break:hang-up? signal) 'SIGHUP]
              [(exn:break:terminate? signal) 'SIGTERM]
              [else 'SIGINT]))
      (syslog-perror 'notice "terminated by ~a." signame)
      (when (symbol=? signame 'SIGHUP)
        (raise signal))))
  
  (define serve-forever : (-> Void)
    (lambda []
      (define root? (zero? (getuid)))
      (with-handlers ([exn:foreign? (exit-with 'EPERM)])
        (seteuid (getuid))
        (setegid (getgid)))

      (define read-request : Read-Request (make-read-request #:connection-close? #false))
      (define initial-connection-timeout : Integer (sakuyamon-connection-timeout))
      
      (define-values (shutdown portno)
        (with-handlers ([exn:fail:network? (exit-with 'FATAL)])
          (parameterize ([current-server-custodian (make-custodian)])
            (define tm : Timer-Manager (start-timer-manager))
            (tcp-server (or (sakuyamon-port) 80) ; TODO: Racket OpenSSL is buggy?
                        #:max-allow-wait (sakuyamon-max-waiting)
                        #:localhost #false
                        #:timeout #false
                        #:on-error (λ [[e : exn]] (syslog-perror 'error "~a" (exn-message e)))
                        #:custodian (current-server-custodian)
                        (λ [[ip : Input-Port] [op : Output-Port] [portno : Positive-Index]]
                          (define conn : Connection
                            (connection 0 ; id, useless but for debugging
                                        (start-timer tm initial-connection-timeout
                                                     (thunk (kill-connection! conn)))
                                        ip op (current-custodian) #f))
                          (with-handlers ([exn:fail:network:errno?
                                           (λ [[e : exn:fail:network:errno]]
                                             (when (= (car (exn:fail:network:errno-errno e)) ECONNRESET)
                                               (kill-connection! conn)))]
                                          [exn:fail?
                                           (λ [[e : exn:fail]]
                                             (when (string=? "fprintf: output port is closed" (exn-message e))
                                               (kill-connection! conn)))])
                            (let read-dispatch-response-loop : Any ()
                              (match (sync (handle-evt (port-closed-evt ip) (λ _ eof))
                                           (peek-bytes-evt 1 0 #false ip))
                                [(? eof-object?) (kill-connection! conn)]
                                [_ (let-values ([(req close?) (read-request conn portno tcp-addresses)])
                                     (dispatch conn req)
                                     (cond [(false? close?) (read-dispatch-response-loop)]
                                           [else (kill-connection! conn)]))]))))))))
      
      ((inst dynamic-wind Void)
       (thunk (void))
       (thunk (with-handlers ([exn:break? signal-handler])
                (when root?
                  (with-handlers ([exn:foreign? (exit-with 'EPERM)])
                    (define-values (uid gid) (fetch_tamer_ids #"tamer"))
                    ;;; if change uid first, then gid cannot be changed again.
                    (setegid gid)
                    (seteuid uid)))
                (when (zero? (geteuid)) ((exit-with 'ECONFIG) "Misconfigured: Privilege Has Not Dropped!"))
                (syslog-perror 'notice "listening on ~a ~a SSL." portno (if (sakuyamon-ssl?) "with" "without"))
                (sync/enable-break never-evt)))
       (thunk (shutdown)))))
    
  ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any Void) (Listof String) (-> String Void) Void))
   (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
   (current-command-line-arguments)
   `([usage-help ,(format "~a~n" desc)]
     [once-each [("-p") ,(λ [[_ : String] [port : String]] (sakuyamon-port (cast (string->number port) (Option Index))))
                        ("Use an alternative <port>." "port")]
                [("-w") ,(λ [[_ : String] [mw : String]] (sakuyamon-max-waiting (cast (string->number mw) Positive-Index)))
                        ("Maximum number of clients can be waiting for acceptance." "mw")]
                [("-t") ,(λ [[_ : String] [ict : String]] (sakuyamon-connection-timeout (cast (string->number ict) Positive-Index)))
                        ("Initial connection timeout." "ict")]
                [("--SSL") ,(λ [[_ : String]] (sakuyamon-ssl? #true)) ("Enable SSL with 443 as default port.")]
                [("--TAMER") ,(λ [[_ : String]] (sakuyamon-tamer-terminus? #true)) ("Enable Per-Tamer Terminus.")]
                [("--DIGIMON") ,(λ [[_ : String]] (sakuyamon-digimon-terminus? #true)) ("Enable Per-Digimon Terminus.")]])
   (λ [!] (serve-forever))
   null
   (lambda [[-h : String]]
     (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
     (exit 0))))
