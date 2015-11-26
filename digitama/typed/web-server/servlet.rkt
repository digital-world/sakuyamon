#lang typed/racket

(require typed/net/url)
(require typed/web-server/http)

(provide Can-Be-Response)
(define-type Can-Be-Response Response)

(require/typed/provide web-server/servlet/servlet-structs
                       [any->response (-> Any (Option Response))]
                       [set-any->response! (-> (-> Any (Option Response)) Void)])

(require/typed/provide web-server/servlet/web
                       [servlet-prompt Any] ; (Prompt-Tagof Any Any)
                       [send/back (-> Can-Be-Response Void)]
                       [current-servlet-continuation-expiration-handler
                        (Parameterof (Option (-> Request Can-Be-Response)))]

                       [send/suspend (-> (-> String Can-Be-Response) Request)]
                       [send/suspend/url (-> (-> URL Can-Be-Response) Request)]
                       [send/suspend/dispatch (-> ((Request -> Any) -> String) Can-Be-Response)]
                       [send/suspend/url/dispatch (-> ((Request -> Any) -> URL) Can-Be-Response)]
                       [send/forward (-> (-> String Can-Be-Response) Request)]
                       [send/finish (-> Can-Be-Response Void)]
                       [redirect/get (-> [#:headers (Listof Header)] Request)]
                       [redirect/get/forget (-> [#:headers (Listof Header)] Request)]

                       [clear-continuation-table! (-> Void)]
                       [with-errors-to-browser (-> (-> Can-Be-Response Request) (-> Any) Any)]
                       [continuation-url? (-> URL (Option (List Number Number Number)))])

(require/typed/provide web-server/servlet/web-cells
                       [#:opaque Web-Cell web-cell?]
                       [make-web-cell (-> Any Web-Cell)]
                       [web-cell-ref (-> Web-Cell Any)]
                       [web-cell-shadow (-> Web-Cell Any Void)])
