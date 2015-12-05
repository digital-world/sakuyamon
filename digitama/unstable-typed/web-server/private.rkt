#lang typed/racket

(provide (all-defined-out))

(require typed/net/url)

(define-type Timer timer)
(define-type Connection connection)
(define-type Path-Piece (U Path-String 'up 'same))

(require/typed/provide web-server/private/web-server-structs
                       [current-server-custodian (Parameterof Custodian)]
                       [make-servlet-custodian (-> Custodian)])

(require/typed/provide web-server/private/timer
                       [#:opaque Timer-Manager timer-manager?]
                       [#:struct timer
                                 ([tm : Timer-Manager]
                                  [evt : (Rec x (Evtof x))]
                                  [expire-seconds : Number]
                                  [action : (-> Void)])
                                 #:extra-constructor-name make-timer]
                       [start-timer-manager (-> Timer-Manager)]
                       [start-timer (-> Timer-Manager Number (-> Void) Timer)]
                       [reset-timer! (-> Timer-Manager Number Void)]
                       [increment-timer! (-> Timer-Manager Number Void)]
                       [cancel-timer! (-> Timer-Manager Void)])

(require/typed/provide web-server/private/connection-manager
                       [#:struct connection
                                 ([id : Number]
                                  [timer : Timer]
                                  [i-port : Input-Port]
                                  [o-port : Output-Port]
                                  [custodian : Custodian]
                                  [close? : Boolean])
                                 #:extra-constructor-name make-connection]
                       [kill-connection! (-> Connection Void)]
                       [adjust-connection-timeout! (-> Connection Number Void)])

(require/typed/provide web-server/private/mod-map
                       [compress-serial (-> (Listof Any) (Listof Any))]
                       [decompress-serial (-> (Listof Any) (Listof Any))])

(require/typed/provide web-server/private/url-param
                       [insert-param (-> URL String String URL)]
                       [extract-param (-> URL String (Option String))])

(require/typed/provide web-server/private/mime-types
                       [read-mime-types (-> Path-String (HashTable Symbol Bytes))]
                       [make-path->mime-type (-> Path-String (-> Path (Option Bytes)))])

(require/typed/provide web-server/private/gzip
                       [gzip/bytes (-> Bytes Bytes)]
                       [gunzip/bytes (-> Bytes Bytes)])

(require/typed/provide web-server/private/util
                       [bytes-ci=? (-> Bytes Bytes Boolean)]
                       [url-replace-path (-> (Listof Path/Param) (Listof Path/Param) URL)]
                       [url-path->string (-> (Listof Path/Param) String)]
                       [explode-path* (-> Path-String (Listof Path-Piece))]
                       [path-without-base (-> Path-String Path-String (Listof Path-Piece))]
                       [directory-part (-> Path-String Path)]
                       [build-path-unless-absolute (-> Path-String Path-String Path)]
                       [network-error (-> Symbol String Any * Nothing)]
                       [exn->string (-> Any String)]
                       [read/bytes (-> Bytes Any)]
                       [write/bytes (-> Any Bytes)])
