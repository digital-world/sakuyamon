#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server with Listening All Addresses")

(module+ sakuyamon
  (require racket/unit)
  (require racket/path)
  (require racket/string)
  (require racket/cmdline)
  (require racket/promise)
  (require racket/function)
  (require racket/match)
  (require racket/place)
  (require racket/async-channel)

  (require syntax/location)
  
  (require net/tcp-sig)
  (require web-server/web-server)
  (require web-server/private/dispatch-server-sig)
  (require web-server/private/dispatch-server-unit)

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/dispatch.rkt")
  (require "../../digitama/daemon.rkt")

  (define {syslog-perror severity maybe . argl}
    (define message (apply format maybe argl))
    (define topic 'realize)
    (with-handlers ([exn:fail? void]) ;;; maybe broken pipe or closed port
      (case severity
        [{notice} (and (printf "~a: ~a~n" topic message)
                       (flush-output))]
        [else (eprintf "~a: ~a~n" topic message)]))
    (rsyslog (severity.c severity) topic message))

  (define {exit-with-eperm tips no}
    (syslog-perror 'error "~a error: ~a; errno=~a" tips (strerror no) no)
    (exit 'EPERM))

  (define {signal-handler signal}
    (unless (port-closed? (current-output-port)) (newline))
    (syslog-perror 'notice "terminated by ~a."
                   (cond [(exn:break:hang-up? signal) 'SIGHUP]
                         [(exn:break:terminate? signal) 'SIGTERM]
                         [else 'SIGINT]))
    (when (exn:break:hang-up? signal) (raise signal)))
  
  (define {serve-forever}
    (define-compound-unit sakuyamon@
      (import)
      (export DS)
      (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
            [{{T : tcp^}} (force sakuyamon-tcp@) DS]
            [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
    (define-values/invoke-unit/infer sakuyamon@)

    (define root? (zero? (getuid)))
    (unless (zero? (seteuid (getuid))) (exit-with-eperm 'regain-uid (saved-errno)))
    (unless (zero? (setegid (getgid))) (exit-with-eperm 'regain-gid (saved-errno)))
    (define-values {errno uid gid} (fetch_tamer_ids #"tamer"))
    (when (and root? (not (zero? errno))) (exit-with-eperm 'fetch-tamer-id errno))

    (define sakuyamon-pipe (make-async-channel #false))
    (define shutdown (parameterize ([error-display-handler void]) (serve #:confirmation-channel sakuyamon-pipe)))
    (define confirmation (async-channel-get sakuyamon-pipe))
    
    (when (exn:fail:network:errno? confirmation)
      (syslog-perror 'error (exn-message confirmation))
      (exit 'FATAL))

    ((λ [fv] (dynamic-wind void fv shutdown))
     (thunk (with-handlers ([exn:break? signal-handler])
              (when root?
                ;;; if change uid first, then gid cannot be changed again.
                (unless (zero? (setegid gid)) (exit-with-eperm 'drop-gid (saved-errno)))
                (unless (zero? (seteuid uid)) (exit-with-eperm 'drop-uid (saved-errno))))
              (when (zero? (geteuid))
                (syslog-perror 'error "Misconfigured: Privilege Has Not Dropped!")
                (exit 'ECONFIG))
              (syslog-perror 'notice "listening on ~a ~a SSL." confirmation (if (sakuyamon-ssl?) "with" "without"))
              (do-not-return)))))

  (parse-command-line (format "~a ~a" (cadr (quote-module-name)) (path-replace-suffix (file-name-from-path (quote-source-file)) #""))
                      (current-command-line-arguments)
                      `{{usage-help ,(format "~a~n" desc)}
                        {once-each [{"-p"} ,{λ [flag port] (sakuyamon-port (string->number port))}
                                           {"Use an alternative <port>." "port"}]
                                   [{"-w"} ,{λ [flag mw] (sakuyamon-max-waiting (string->number mw))}
                                           {"Maximum number of clients can be waiting for acceptance." "mw"}]
                                   [{"-t"} ,{λ [flag ict] (sakuyamon-connection-timeout (string->number ict))}
                                           {"Initial connection timeout." "ict"}]
                                   [{"--SSL"} ,{λ [flag] (sakuyamon-ssl? #true)} {"Enable SSL with 443 as default port."}]
                                   [{"--TAMER"} ,{λ [flag] (sakuyamon-tamer-terminus? #true)} {"Enable Per-Tamer Terminus."}]
                                   [{"--DIGIMON"} ,{λ [flag] (sakuyamon-digimon-terminus? #true)} {"Enable Per-Digimon Terminus."}]}}
                      {λ [!] (serve-forever)}
                      null
                      (compose1 exit display (curryr string-replace #px"  -- : .+?-h --'\\s*" ""))))
