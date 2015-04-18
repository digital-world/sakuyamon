#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server")

{module+ sakuyamon
  (require racket/unit)
  (require racket/string)
  (require racket/cmdline)
  (require racket/async-channel)
  
  (require net/tcp-sig)
  (require web-server/private/dispatch-server-sig)
  (require web-server/private/dispatch-server-unit)

  (require "../../digitama/configuration.rkt")
  
  (parse-command-line "sakuyamon realize" (current-command-line-arguments)
                      `{{usage-help ,(format "~a~n" desc)}}
                      {λ _ (let ([ping (make-async-channel #false)])
                             (define-compound-unit sakuyamon@
                               (import)
                               (export DS)
                               (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
                                     [{{T : tcp^}} sakuyamon-tcp@ DS]
                                     [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
                             
                             (define-values/invoke-unit/infer sakuyamon@)

                             (define shutdown (parameterize ([error-display-handler {λ [brief netexn] (eprintf "~a~n" brief)}])
                                                (serve #:confirmation-channel ping)))
                             (define pinged (async-channel-get ping))
                             
                             (dynamic-wind void
                                           {λ _ (cond [(exn:fail:network:errno? pinged) (exit (car (exn:fail:network:errno-errno pinged)))] 
                                                      [else (with-handlers ([exn:break? {λ _ (newline)}])
                                                              (printf "sakuyamon: mission start~n")
                                                              (let do-not-return ()
                                                                (define which (sync (if (terminal-port? /dev/stdin) /dev/stdin ping)))
                                                                (unless (and (eq? which /dev/stdin) (eof-object? (read-line /dev/stdin)))
                                                                  (do-not-return))))])}
                                           shutdown))}
                      null
                      {λ [--help] (exit (display (string-replace --help #px"  -- : .+?-h --'\\s*" "")))})}
