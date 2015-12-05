#lang typed/racket

(provide (all-defined-out))

(require/typed/provide racket/serialize
                       [#:opaque Serializable serializable?])

; NOTE: stuffer/c is something that Typed Racket cannot work with;
;       therefore, clients should not make its instance directly
;       unless the 'Stuffer Laws' are checked mannually. Otherwise
;       the exists ones, such as `serialize-stuffer` or `hash-stuffer`
;       and so on, should work for most cases.
                       
(define-type (Stuffer Tdom Trng) stuffer) ; make it more meaningful for human readers

(define-type Store store)
(define-type Hash-Fun (-> Bytes Bytes))

(require/typed/provide web-server/stuffers
                       [#:struct stuffer
                                 ([in : (-> Any Any)]
                                  [out : (-> Any Any)])
                                 #:extra-constructor-name make-stuffer]
                       [stuffer-compose (-> (Stuffer Any Any) (Stuffer Any Any) (Stuffer Any Any))]
                       [stuffer-sequence (-> (Stuffer Any Any) (Stuffer Any Any) (Stuffer Any Any))]
                       [stuffer-if (-> (-> Bytes Boolean) (Stuffer Bytes Bytes) (Stuffer Bytes Bytes))]
                       [stuffer-chain (-> (U stuffer (-> Bytes Boolean)) * stuffer)]

                       [id-stuffer (Stuffer Any Any)]
                       [serialize-stuffer (Stuffer Serializable Bytes)]
                       [base64-stuffer (Stuffer Bytes Bytes)]
                       [gzip-stuffer (Stuffer Bytes Bytes)]
                       [default-stuffer (Stuffer Serializable Bytes)]
                       
                       [#:struct store
                                 ([write : (Bytes Bytes -> Void)]
                                  [read : (Bytes -> Bytes)])
                                 #:extra-constructor-name make-store]
                       [dir-store (-> Path-String Store)]
                       [hash-stuffer (-> Hash-Fun Store (Stuffer Bytes Bytes))]
                       [md5-stuffer (-> Path-String (Stuffer Bytes Bytes))]

                       [HMAC-SHA1 (-> Bytes Bytes Bytes)]
                       [HMAC-SHA1-stuffer (-> Bytes (Stuffer Bytes Bytes))]

                       [is-url-too-big? (-> Bytes Boolean)]
                       [make-default-stuffer (-> Path-String (Stuffer Serializable Bytes))])
