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
      (case severity
        [{notice} (printf "~a: ~a~n" topic message)]
        [else (eprintf "~a: ~a~n" topic message)])
      (rsyslog (severity.c severity) topic message)})
  
  (define {serve-forever}
    (define-compound-unit sakuyamon@
      (import)
      (export DS)
      (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
            [{{T : tcp^}} (force sakuyamon-tcp@) DS]
            [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
    (define-values/invoke-unit/infer sakuyamon@)

    (define daemon? (eq? (getppid) 1)) ;;; child of the init process. Maybe BUG for zombie process.
    (define-values {errno uid gid} (fetch_tamer_ids #"tamer"))
    (define exit-with-errno {λ [no] (exit ({λ _ no} (syslog 'error "system error: ~a; errno=~a~n" (strerror no) no)))})
    
    (define sakuyamon-pipe (make-async-channel #false))
    (define shutdown (parameterize ([error-display-handler void]) (serve #:confirmation-channel sakuyamon-pipe)))
    (define confirmation (async-channel-get sakuyamon-pipe))

    (dynamic-wind {λ _ (void)}
                  {λ _ (cond [(and (exn:fail:network:errno? confirmation) confirmation)
                              => (compose1 exit {λ _ (car (exn:fail:network:errno-errno confirmation))} (curry syslog-perror 'error) exn-message)]
                             [(and daemon? (not (zero? errno)) errno)
                              => exit-with-errno]
                             [else (with-handlers ([exn:break? {λ _ (unless (port-closed? (current-output-port)) (newline))}])
                                     (when daemon? ; I am root
                                       ;;; setuid would drop the privilege of seting gid.
                                       (unless (zero? (setgid gid)) (exit-with-errno (saved-errno)))
                                       (unless (zero? (setuid uid)) (exit-with-errno (saved-errno))))
                                     (when (zero? (getuid))
                                       (syslog-perror 'error "misconfigured: Privilege Has Not Dropped!"))
                                     (syslog-perror 'notice "listen on ~a ~a SSL~n" confirmation (if (sakuyamon-ssl?) "with" "without"))
                                     (when (place-channel? (tamer-pipe)) ;;; for testing
                                       (place-channel-put (tamer-pipe) (list (sakuyamon-ssl?) confirmation)))
                                     (cond [daemon? (do-not-return)]
                                           [else (let do-not-return ([stdin /dev/stdin])
                                                   (unless (eof-object? (read-line stdin))
                                                     (sync/enable-break (handle-evt stdin do-not-return))))]))])}
                  {λ _ (shutdown)}))

  (call-as-normal-termination
   {λ _ (parse-command-line (format "~a ~a" (cadr (quote-module-name)) (path-replace-suffix (file-name-from-path (quote-source-file)) #""))
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
                            (compose1 exit display (curryr string-replace #px"  -- : .+?-h --'\\s*" "")))})}

{module digitama racket/base
  (provide (all-defined-out))

  (define tamer-pipe (make-parameter #false))}
