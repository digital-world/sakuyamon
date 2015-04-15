#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server")

{module sakuyamon racket/base
  (require racket/unit)
  (require racket/async-channel)
  
  (require net/tcp-sig)
  (require web-server/private/dispatch-server-sig)
  (require web-server/private/dispatch-server-unit)

  (require "../../digitama/configuration.rkt")
  
  (define-compound-unit sakuyamon@
    (import)
    (export DS)
    (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
          [{{T : tcp^}} sakuyamon-tcp@ DS]
          [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
  (define-values/invoke-unit/infer sakuyamon@)

  (let* ([ping (make-async-channel #false)]
         [shutdown (parameterize ([error-display-handler void])
                     (serve #:confirmation-channel ping))]
         [pinged (async-channel-get ping)])
    (cond [(exn? pinged) (printf "~a~n" (exn-message pinged))] 
          [else (dynamic-wind {λ _ (printf "sakuyamon: mission start~n")}
                              {λ _ (with-handlers ([exn:break? {λ _ (newline)}])
                                     #|do-not-return|# (async-channel-get ping))}
                              {λ _ (printf "sakuyamon: mission complete!~n")})])
    (void (shutdown)))}
