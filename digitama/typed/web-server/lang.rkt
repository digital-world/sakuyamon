#lang typed/racket

(provide (all-from-out typed/net/url typed/web-server/http))
(provide (all-from-out "stuffers.rkt"))

(require typed/net/url)
(require typed/web-server/http)

(require "servlet.rkt")
(require "stuffers.rkt")

(require/typed/provide racket/base
                       [#:opaque Continuation continuation?])

(require/typed/provide web-server/dispatch/extend ; TODO: syntax
                       [make-coerce-safe? (-> (-> Any Any) (-> Any Boolean))])

(require/typed/provide web-server/lang/abort-resume ; TODO: syntax
                       [call-with-serializable-current-continuation (-> Continuation Any)])

(require/typed/provide web-server/lang/native) ; TODO: syntax

(require/typed/provide web-server/lang/web
                       [send/suspend (-> (-> String Response) Request)]
                       [send/suspend/hidden (-> (-> URL Any #|xexpr/c|# Response) Request)]
                       [send/suspend/url (-> (-> URL Response) Request)]
                       [send/suspend/dispatch (-> ((Request -> Any) -> String) Response)]
                       [send/suspend/url/dispatch (-> ((Request -> Any) -> URL) Response)]
                       [redirect/get (-> Request)])

(require/typed/provide web-server/lang/web-cells ; TODO: syntax
                       [#:opaque Web-Cell web-cell?]
                       [web-cell-ref (-> Web-Cell Any)]
                       [web-cell-shadow (-> Web-Cell Any Void)])

(require/typed/provide web-server/lang/file-box
                       [#:opaque File-Box file-box?]
                       [file-box (-> Path-String Serializable File-Box)]
                       [file-unbox (-> File-Box Serializable)]
                       [file-box-set? (-> File-Box Boolean)]
                       [file-box-set! (-> File-Box Serializable Void)])

(require/typed/provide web-server/lang/web-param ; TODO: syntax
                       [#:opaque Web-Parameter web-parameter?])

(require/typed/provide web-server/lang/soft
                       [#:opaque Soft-State soft-state?]
                       [make-soft-state (-> (-> Any) Soft-State)]
                       [soft-state-ref (-> Soft-State Any)])

(require/typed/provide web-server/dispatch ; TODO: syntax
                       [#:opaque Container container?]
                       [serve/dispatch (-> Request Can-Be-Response)])
