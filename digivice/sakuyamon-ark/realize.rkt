#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server")

{module sakuyamon racket/base
  (require net/tcp-sig)
  (require (only-in web-server/web-server do-not-return))
  (require web-server/private/dispatch-server-sig)
  (require web-server/private/dispatch-server-unit)
 
  (require "../../digitama/sakuyamon/configuration.rkt")

  (define-compound-unit sakuyamon@
    (import)
    (export DS)
    (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
          [{{T : tcp^}} sakuyamon-tcp@ DS]
          [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))

  (define-values/invoke-unit/infer sakuyamon@)
  (void (serve))
  (do-not-return)}
