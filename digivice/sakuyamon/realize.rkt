#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server Listening All Addresses")

{module+ sakuyamon
  (require racket/unit)
  (require racket/string)
  (require racket/cmdline)
  (require racket/promise)
  (require racket/async-channel)
  
  (require net/tcp-sig)
  (require web-server/private/dispatch-server-sig)
  (require web-server/private/dispatch-server-unit)

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/configuration.rkt")
  
  (parse-command-line "sakuyamon realize" (current-command-line-arguments)
                      `{{usage-help ,(format "~a~n" desc)}
                        {once-each [{"-p"} ,{λ [flag port] (sakuyamon-port (string->number port))}
                                           {"Use an alternative <port>." "port"}]
                                   [{"-w"} ,{λ [flag mw] (sakuyamon-max-waiting (string->number mw))}
                                           {"Maximum number of clients can be waiting for acceptance." "mw"}]
                                   [{"-t"} ,{λ [flag ict] (sakuyamon-connection-timeout (string->number ict))}
                                           {"Initial connection timeout." "ict"}]
                                   [{"--ssl"} ,{λ [flag] (sakuyamon-ssl? #true)} {"Enable SSL with 443 as default port."}]}}
                      {λ _ (let ([ping (make-async-channel #false)])
                             (define-compound-unit sakuyamon@
                               (import)
                               (export DS)
                               (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
                                     [{{T : tcp^}} (force sakuyamon-tcp@) DS]
                                     [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
                             
                             (define-values/invoke-unit/infer sakuyamon@)

                             (define shutdown (parameterize ([error-display-handler {λ [brief netexn] (eprintf "~a~n" brief)}])
                                                (serve #:confirmation-channel ping)))

                             (define pinged (async-channel-get ping))
                             (dynamic-wind void
                                           {λ _ (cond [(and (exn:fail:network:errno? pinged) pinged)
                                                       => (compose1 exit car exn:fail:network:errno-errno)] 
                                                      [else (with-handlers ([exn:break? {λ _ (newline)}])
                                                              (printf "sakuyamon: mission start [@::~a]~n" pinged)
                                                              (let do-not-return ()
                                                                (define which (sync /dev/stdin))
                                                                (unless (eof-object? (read-line /dev/stdin))
                                                                  (do-not-return))))])}
                                           shutdown))}
                      null
                      {λ [--help] (exit (display (string-replace --help #px"  -- : .+?-h --'\\s*" "")))})}
