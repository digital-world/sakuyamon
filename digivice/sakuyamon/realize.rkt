#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server with Listening All Addresses")

{module+ sakuyamon
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

  (require (submod ".." digitama))
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/dispatch.rkt")
  (require "../../digitama/posix.rkt")

  (define syslog-perror
    {lambda [severity maybe . argl]
      (define message (apply format maybe argl))
      (define topic 'realize)
      (unless (port-closed? (current-output-port))
        (case severity
          [{notice} (printf "~a: ~a~n" topic message)]
          [else (eprintf "~a: ~a~n" topic message)]))
      (rsyslog (severity.c severity) topic message)})
  
  (define {serve-forever}
    (define-compound-unit sakuyamon@
      (import)
      (export DS)
      (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
            [{{T : tcp^}} (force sakuyamon-tcp@) DS]
            [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
    (define-values/invoke-unit/infer sakuyamon@)

    (define daemon? (zero? (getuid))) ;;; I am root
    (define-values {errno uid gid} (fetch_tamer_ids #"tamer"))
    (define exit-with-errno {λ [no] (exit ({λ _ no} (syslog-perror 'error "system error: ~a; errno=~a" (strerror no) no)))})
    
    (define sakuyamon-pipe (make-async-channel #false))
    (define shutdown (parameterize ([error-display-handler void]) (serve #:confirmation-channel sakuyamon-pipe)))
    (define confirmation (async-channel-get sakuyamon-pipe))

    (dynamic-wind {λ _ (void)}
                  {λ _ (cond [(and (exn:fail:network:errno? confirmation) confirmation)
                              => (compose1 exit {λ _ (car (exn:fail:network:errno-errno confirmation))} (curry syslog-perror 'error) exn-message)]
                             [(and daemon? (not (zero? errno)) errno)
                              => exit-with-errno]
                             [else (with-handlers ([exn:break? {λ [signal] (void (unless (port-closed? (current-output-port)) (newline))
                                                                                 (syslog-perror 'notice "terminated by ~a."
                                                                                                (cond [(exn:break:hang-up? signal) 'SIGHUP]
                                                                                                      [(exn:break:terminate? signal) 'SIGTERM]
                                                                                                      [else 'SIGINT]))
                                                                                 (when (exn:break:hang-up? signal) (raise signal)))}])
                                     (when daemon?
                                       ;;; if change uid first, then gid cannot be changed again.
                                       (unless (zero? (setegid gid)) (exit-with-errno (saved-errno)))
                                       (unless (zero? (seteuid uid)) (exit-with-errno (saved-errno))))
                                     (when (zero? (geteuid))
                                       (syslog-perror 'error "Misconfigured: Privilege Has Not Dropped!"))
                                     (syslog-perror 'notice "listen on ~a ~a SSL." confirmation (if (sakuyamon-ssl?) "with" "without"))
                                     (when (place-channel? (tamer-pipe)) ;;; for testing
                                       (place-channel-put (tamer-pipe) (list (sakuyamon-ssl?) confirmation)))
                                     (do-not-return))])}
                  {λ _ (void (unless (zero? (seteuid (getuid))) (exit-with-errno (saved-errno)))
                             (unless (zero? (setegid (getgid))) (exit-with-errno (saved-errno)))
                             (shutdown))}))

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
                      {λ [!] (serve-forever)} null
                      (compose1 exit display (curryr string-replace #px"  -- : .+?-h --'\\s*" "")))}

{module digitama racket/base
  (provide (all-defined-out))

  (define tamer-pipe (make-parameter #false))}
