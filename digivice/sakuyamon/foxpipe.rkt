#lang typed/racket

(provide (all-defined-out))

(define desc : String "Launch the rsyslog proxy server")

(module+ sakuyamon
  (require "../../digitama/digicore.rkt")
  
  (require "../../digitama/posixed.rkt")

  (require/typed racket/base
                 [exn:break:hang-up? (-> Any Boolean)]
                 [exn:break:terminate? (-> Any Boolean)])

  (define max-size 65535)
  (define log-pool (make-bytes max-size))
  
  (define signal-handler : (-> exn Void)
    (lambda [signal]
      (unless (port-closed? (current-output-port)) (newline))
      (syslog 'notice 'foxpipe "terminated by ~a."
              (cond [(exn:break:hang-up? signal) 'SIGHUP]
                    [(exn:break:terminate? signal) 'SIGTERM]
                    [else 'SIGINT]))
      (when (exn:break:hang-up? signal) (raise signal))))
  
  (define serve-forever : (-> (Evtof (List Natural String Natural)) Void)
    (lambda [/dev/udp]
      (with-handlers ([exn:break? signal-handler])
        (match (sync/enable-break /dev/udp)
          [{list size _ _} (displayln (bytes->string/utf-8 log-pool #false 0 size))])
        #|
                                   (with-handlers ([exn? displayln])
                                     (define-values {/dev/login /dev/logout}
                                       (tcp-accept/enable-break listener))
                                     (thread (thunk (dynamic-wind void
                                                                  (thunk (with-handlers ([exn? displayln])
                                                                           (tcp-read /dev/login /dev/logout)))
                                                                  (thunk (for-each tcp-abandon-port
                                                                                   (list /dev/login /dev/logout))))))))|#
        (serve-forever /dev/udp))))

  (define-type HELP (Listof (List Symbol (U String (List (Listof String) Any (Listof String))))))
  ((cast parse-command-line (-> Path-String (Vectorof String) HELP (-> Any Void) (List) (-> String Void) Void))
   (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
   (current-command-line-arguments)
   `{{usage-help ,(format "~a~n" desc)}}
   (lambda [[flags : Any]]
     (parameterize ([current-custodian (make-custodian)])
       (define exit-with-eperm : (-> Symbol Natural Nothing)
         (lambda [tips no]
           (syslog 'error 'foxpipe "~a error: ~a; errno=~a" tips (strerror no) no)
           (exit 'EPERM)))
       
       (define foxpipe (udp-open-socket))
       (dynamic-wind (thunk (let ([root? (zero? (getuid))])
                              (unless (zero? (seteuid (getuid))) (exit-with-eperm 'regain-uid (saved-errno)))
                              (unless (zero? (setegid (getgid))) (exit-with-eperm 'regain-gid (saved-errno)))
                              (define-values {errno uid gid} (fetch_tamer_ids #"tamer"))
                              (when (and root? (not (zero? errno))) (exit-with-eperm 'fetch-tamer-id errno))
                              
                              (with-handlers ([exn:fail:network? void])
                                (udp-bind! foxpipe "127.0.0.1" (or (sakuyamon-foxpipe-port) 514)))
                              
                              (when root?
                                ;;; if change uid first, then gid cannot be changed again.
                                (unless (zero? (setegid gid)) (exit-with-eperm 'drop-gid (saved-errno)))
                                (unless (zero? (seteuid uid)) (exit-with-eperm 'drop-uid (saved-errno))))
                              
                              (when (zero? (geteuid))
                                (syslog 'error 'foxpipe "Misconfigured: Privilege Has Not Dropped!")
                                (exit 'ECONFIG))
                              
                              (match-let-values ([{_ port _ _} (udp-addresses foxpipe #true)])
                                (syslog 'notice 'foxpipe "waiting rsyslog packets on ~a." port))))
                     (thunk (serve-forever (udp-receive!-evt foxpipe log-pool)))
                     (thunk (custodian-shutdown-all (current-custodian))))))
   null
   (lambda [[-h : String]]
     (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
     (exit 0))))
