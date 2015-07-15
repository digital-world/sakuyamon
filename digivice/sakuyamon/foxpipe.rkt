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
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  
  (define foxlog : (-> Symbol String Any * Void)
    (lambda [severity maybe . argl]
      (rsyslog severity 'foxpipe (apply format maybe argl))))

  (define syslog-perror : (-> String Any * Void)
    (lambda [maybe . argl]
      (define errmsg : String (apply format maybe argl))
      (with-handlers ([exn:fail? void])
        (eprintf "~a~n" errmsg))
      (foxlog 'error errmsg)))
  
  (define exit-with-eperm : (-> exn:foreign Nothing)
    (lambda [efe]
      (syslog-perror "~a" (exn-message efe))
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
      (thread-wait fox)
      (when (exn:break:hang-up? signal)
        (raise signal))))

  (define remove-port : (-> Input-Port Symbol Void)
    (lambda [/dev/tcpin reason]
      (define /dev/tcpout ((inst hash-ref Input-Port Output-Port Nothing) izunas (cast /dev/tcpin Input-Port)))
      (match-define-values [_ _ remote port] (tcp-addresses /dev/tcpout #true))
      ((inst hash-remove! Input-Port Output-Port) izunas (cast /dev/tcpin Input-Port))
      (tcp-abandon-port /dev/tcpin)
      (tcp-abandon-port /dev/tcpout)
      (foxlog 'notice "~a:~a has ~a!" remote port (case reason
                                                    [(eof) 'gone]
                                                    [(exn) 'broken]))))
  
  (define push-back : (-> Any Void)
    (lambda [packet]
      (for ([{/dev/iznin /dev/iznout} ((inst in-hash Input-Port Output-Port) izunas)])
        (with-handlers ([exn? (lambda [e] (remove-port /dev/iznin 'exn))])
          (match packet
            [(? string?) (write (string-split packet (~a #\newline)) /dev/iznout)]
            [_ (write packet /dev/iznout)])
          (flush-output /dev/iznout)))))
  
  (define serve-forever : (-> (Evtof (List Natural String Natural)) (Evtof (List Input-Port Output-Port)) Void)
    (lambda [/dev/udp /dev/tcp]
      (match (apply sync/timeout/enable-break (sakuyamon-foxpipe-idle) /dev/udp /dev/tcp
                    (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive)))
                    ((inst hash-keys Input-Port Output-Port) izunas))
        [(list /dev/tcpin /dev/tcpout)
         (let-values ([{local lport remote port} (tcp-addresses (cast /dev/tcpin Port) #true)])
           (foxlog 'notice  "~a:~a has connected via SSH Channel." remote port)
           ((inst hash-set! Input-Port Output-Port) izunas (cast /dev/tcpin Input-Port) (cast /dev/tcpout Output-Port)))]
        [(list size _ _)
         (when (or (terminal-port? (current-output-port)) (positive? (hash-count izunas)))
           (define packet : String (string-trim (bytes->string/utf-8 log-pool #false 0 (cast size Natural))))
           (with-handlers ([exn:fail? void])
             (displayln packet)
             (flush-output (current-output-port)))
           (push-back packet))]
        [(? tcp-port? /dev/tcpin) (remove-port (cast /dev/tcpin Input-Port) 'eof)]
        [(? exn:break? signal) (raise signal)]
        [#false (push-back beating-heart#)])
      (serve-forever /dev/udp /dev/tcp)))

  ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any Void) (Listof String) (-> String Void) Void))
   (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
   (current-command-line-arguments)
   `([usage-help ,(format "~a~n" desc)])
   (lambda [[flags : Any]]
     (parameterize ([current-custodian (make-custodian)])
       (dynamic-wind (thunk (with-handlers ([exn:foreign? exit-with-eperm])
                              (seteuid (getuid))
                              (setegid (getgid))
                              
                              (with-handlers ([exn:fail:network? exit-with-fatal])
                                (udp-bind! foxpipe "localhost" (sakuyamon-scepter-port)) ;;; localhost binds both IPv4 and IPv6
                                (scepter (tcp-listen (sakuyamon-scepter-port) (sakuyamon-foxpipe-max-waiting) #true "localhost")))
                              
                              (when root?
                                (define-values {uid gid} (fetch_tamer_ids #"tamer"))
                                ;;; if change uid first, then gid cannot be changed again.
                                (setegid gid)
                                (seteuid uid))
                              
                              (when (zero? (geteuid))
                                (syslog-perror "Misconfigured: Privilege Has Not Dropped!")
                                (exit 'ECONFIG))
                              
                              (match-let-values ([{_ port _ _} (udp-addresses foxpipe #true)])
                                (foxlog 'notice "waiting rsyslog packets on ~a." port))

                              (unless (terminal-port? (current-output-port))
                                (displayln beating-heart#)
                                (flush-output))))
                     (thunk (with-handlers ([exn:break? signal-handler])
                              (kudagitsune (thread (thunk (with-handlers ([exn:break? void])
                                                            (serve-forever (udp-receive!-evt foxpipe log-pool)
                                                                           (tcp-accept-evt (cast (scepter) TCP-Listener)))))))
                              (sync/enable-break never-evt)))
                     (thunk (and (udp-close foxpipe) ;;; custodian dose not care about udp socket.
                                 (custodian-shutdown-all (current-custodian)))))))
   null
   (lambda [[-h : String]]
     (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
     (exit 0))))
