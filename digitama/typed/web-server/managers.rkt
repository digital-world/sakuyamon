#lang typed/racket

(provide (all-defined-out))

(require typed/web-server/http)

(require "servlet.rkt")

(define-type Manager manager)

(require/typed/provide web-server/managers/manager
                       [#:struct manager
                                 ([create-instance : (-> (-> Void) Number)]
                                  [adjust-timeout! : (-> Number Number Void)] #|to-be-deprecated|#
                                  [clear-continuations! : (-> Number Void)]
                                  [continuation-store! : (-> Number
                                                             (Option (Request -> Response))
                                                             (List Number Number))]
                                  [continuation-lookup : (-> Number Number Number Any)]
                                  [continuation-peek : (-> Number Number Number Any)])
                                 #:extra-constructor-name make-manager]
                       [#:struct (exn:fail:servlet-manager:no-instance exn:fail)
                                 ([expiration-handler : (Option (Request -> Response))])
                                 #:extra-constructor-name make-exn:fail:servlet-manager:no-instance]
                       [#:struct (exn:fail:servlet-manager:no-continuation exn:fail)
                                 ([expiration-handler : (Option (Request -> Response))])
                                 #:extra-constructor-name make-exn:fail:servlet-manager:no-continuation])

(require/typed/provide web-server/managers/none
                       [create-none-manager (-> (Option (Request -> Response)) Manager)])

(require/typed/provide web-server/managers/timeouts
                       [create-timeout-manager (-> (Option (Request -> Response))
                                                   Number
                                                   Number
                                                   Manager)])

(require/typed/provide web-server/managers/lru
                       [make-threshold-LRU-manager (-> (Option (Request -> Response)) Number Manager)]
                       [create-LRU-manager (-> (Option (Request -> Response))
                                               Integer
                                               Integer
                                               (-> Boolean)
                                               [#:initial-count Integer]
                                               [#:inform-p (-> Integer Void)]
                                               Manager)])
