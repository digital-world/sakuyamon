#lang typed/racket

(provide (all-defined-out))

(define desc : String "Launch the rsyslog proxy server")

(module+ sakuyamon
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/daemon.rkt")

  (require/typed racket/base
                 [exn:break:hang-up? (-> Any Boolean)]
                 [exn:break:terminate? (-> Any Boolean)])

  (define root? (zero? (getuid)))
  
  (define max-size : Natural 65535)
  (define log-pool : Bytes (make-bytes max-size))
  (define foxpipe : UDP-Socket (udp-open-socket))
  (define scepter : (Parameterof (Option TCP-Listener)) (make-parameter #false))
  
  (define kudagitsune : (Parameterof (Option Thread)) (make-parameter #false))
  (define izunas : (HashTable Input-Port Output-Port) ((inst make-hash Input-Port Output-Port)))
  (define sakuyamon-scepter-port : (Parameterof Positive-Integer)
    (make-parameter (+ (sakuyamon-foxpipe-port) (if root? 0 16180))))
  
  (define foxlog : (-> Symbol String Any * Void)
    (lambda [severity maybe . argl]
      (rsyslog (severity.c severity) 'foxpipe (apply format maybe argl))))

  (define syslog-perror : (-> String Any * Void)
    (lambda [maybe . argl]
      (define errmsg : String (apply format maybe argl))
      (with-handlers ([exn:fail? void])
        (eprintf "~a~n" errmsg))
      (foxlog 'error errmsg)))
  
  (define exit-with-eperm : (-> Symbol Natural Nothing)
    (lambda [tips no]
      (syslog-perror "~a error: ~a; errno=~a" tips (strerror no) no)
      (exit 'EPERM)))

  (define exit-with-fatal : (-> exn Nothing)
    (lambda [e]
      (syslog-perror (exn-message e))
      (exit 'FATAL)))
  
  (define signal-handler : (-> exn Void)
    (lambda [signal]
      (define fox : Thread (cast (kudagitsune) Thread))
      (foxlog 'notice "terminated by ~a."
              (cond [(exn:break:hang-up? signal) 'SIGHUP]
                    [(exn:break:terminate? signal) 'SIGTERM]
                    [else 'SIGINT]))
      (thread-send fox signal)
      (thread-wait fox) ; It should also handle this log before exiting.
      (when (exn:break:hang-up? signal)
        (raise signal))))

  (define identity/timeout : (-> Input-Port Output-Port Any)
    (lambda [/dev/tcpin /dev/tcpout]
      (match-define-values {_ _ remote port} (tcp-addresses /dev/tcpin #true))
      (foxlog 'notice  "~a:~a has connected." remote port)
      ((inst hash-set! Input-Port Output-Port) izunas /dev/tcpin /dev/tcpout)))

  (define push-back : (-> Any Void)
    (lambda [packet]
      (for ([/dev/iznout ((inst in-hash-values Input-Port Output-Port) izunas)])
        (write packet /dev/iznout)
        (flush-output /dev/iznout))))
  
  (define serve-forever : (-> (Evtof (List Natural String Natural)) (Evtof (List Input-Port Output-Port)) Void)
    (lambda [/dev/udp /dev/tcp]
      (match (apply sync/timeout (sakuyamon-foxpipe-idle) /dev/udp /dev/tcp
                    ((inst hash-keys Input-Port Output-Port) izunas))
        [{list /dev/tcpin /dev/tcpout} (thread (thunk (identity/timeout /dev/tcpin /dev/tcpout)))]
        [{list size _ _} (when (or (terminal-port? (current-output-port)) (positive? (hash-count izunas)))
                           (define packet (bytes->string/utf-8 log-pool #false 0 size))
                           (with-handlers ([exn:fail? void])
                             (displayln packet)
                             (flush-output (current-output-port)))
                           (push-back packet))]
        [{? tcp-port? /dev/tcpin} (let ([/dev/tcpout ((inst hash-ref Input-Port Output-Port Nothing) izunas /dev/tcpin)])
                                    ((inst hash-remove! Input-Port Output-Port) izunas /dev/tcpin)
                                    (match-define-values {_ _ remote port} (tcp-addresses /dev/tcpout #true))
                                    (foxlog 'notice "~a:~a has gone!" remote port))]
        [#false (void (push-back beating-heart#)
                      (when (and root? (symbol=? (digimon-system) 'solaris))
                        (when (system (format "sh ~a/foxpipe/kill_if_cpueating.sh" (digimon-stone)))
                          (foxlog 'notice "Sakuyamon has terminated to release the CPU core!"))))])
      (unless (exn:break? (thread-try-receive))
        (serve-forever /dev/udp /dev/tcp))))

  ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any Void) (Listof String) (-> String Void) Void))
   (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
   (current-command-line-arguments)
   `([usage-help ,(format "~a~n" desc)]
     [once-each
      [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Positive-Integer)))
              {"Use an alternative <port>." "port"}]])
   (lambda [[flags : Any]]
     (parameterize ([current-custodian (make-custodian)])
       (dynamic-wind (thunk (let-values ([{errno uid gid} (fetch_tamer_ids #"tamer")])
                              (unless (zero? (seteuid (getuid))) (exit-with-eperm 'regain-uid (saved-errno)))
                              (unless (zero? (setegid (getgid))) (exit-with-eperm 'regain-gid (saved-errno)))
                              (when (and root? (not (zero? errno))) (exit-with-eperm 'fetch-tamer-id errno))
                              
                              (with-handlers ([exn:fail:network? exit-with-fatal])
                                (udp-bind! foxpipe "127.0.0.1" (sakuyamon-scepter-port))
                                (scepter (tcp-listen (sakuyamon-scepter-port) (sakuyamon-foxpipe-max-waiting) #true)))
                              
                              (when root?
                                ;;; if change uid first, then gid cannot be changed again.
                                (unless (zero? (setegid gid)) (exit-with-eperm 'drop-gid (saved-errno)))
                                (unless (zero? (seteuid uid)) (exit-with-eperm 'drop-uid (saved-errno))))
                              
                              (when (zero? (geteuid))
                                (syslog-perror "Misconfigured: Privilege Has Not Dropped!")
                                (exit 'ECONFIG))
                              
                              (match-let-values ([{_ port _ _} (udp-addresses foxpipe #true)])
                                (foxlog 'notice "waiting rsyslog packets on ~a." port))

                              (unless (terminal-port? (current-output-port))
                                (displayln beating-heart#)
                                (flush-output))))
                     (thunk (with-handlers ([exn:break? signal-handler])
                              (kudagitsune (thread (thunk (serve-forever (udp-receive!-evt foxpipe log-pool)
                                                                         (tcp-accept-evt (cast (scepter) TCP-Listener))))))
                              (sync/enable-break never-evt)))
                     (thunk (and (udp-close foxpipe) ;;; custodian dose not care about udp socket.
                                 (custodian-shutdown-all (current-custodian)))))))
   null
   (lambda [[-h : String]]
     (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
     (exit 0))))
