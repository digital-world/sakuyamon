#lang typed/racket

(require typed/net/url)

(provide (all-defined-out))

(define-type Header header)
(define-type Binding binding)
(define-type Request request)
(define-type Response response)

(define-type Digest-Credentials (Listof (Pairof Symbol String)))
(define-type Username*Realm->Password (-> String String String))
(define-type Username*Realm->Digest-HA1 (-> String String Bytes))

(require/typed/provide web-server/http
                       [#:opaque Redirection-Status redirection-status?]
                       [TEXT/HTML-MIME-TYPE Bytes]
                       [#:struct header {[field : Bytes]
                                         [value : Bytes]}
                                 #:extra-constructor-name make-header]
                       [#:struct binding {[id : Bytes]}
                                 #:extra-constructor-name make-binding]
                       [#:struct {binding:form binding} {[value : Bytes]}
                                 #:extra-constructor-name make-binding:form]
                       [#:struct {binding:file binding} {[filename : Bytes]
                                                         [headers : (Listof Header)]
                                                         [content : Bytes]}
                                 #:extra-constructor-name make-binding:file]
                       [#:struct request {[method : Bytes]
                                          [uri : URL]
                                          [headers/raw : (Listof Header)]
                                          [bindings/raw-promise : (Promise (Listof Binding))]
                                          [post-data/raw : (Option Bytes)]
                                          [host-ip : String]
                                          [host-port : Natural]
                                          [client-ip : String]}
                                 #:extra-constructor-name make-request]
                       [#:struct response {[code : Natural]
                                           [message : Bytes]
                                           [seconds : Natural]
                                           [mime : (Option Bytes)]
                                           [headers : (Listof Header)]
                                           [output : (-> Output-Port Void)]}]
                       [headers-assq* (-> Bytes (Listof Header) (Option Header))]
                       [response/full (-> Natural Bytes Number (Option Bytes) (Listof Header) (Listof Bytes) Response)]
                       [response/xexpr (-> Any [#:code Natural] [#:message Bytes] [#:headers (Listof Header)] Response)]
                       [response/output (-> (-> Output-Port Void) [#:code Natural] [#:message Bytes] [#:headers (Listof Header)] Response)]
                       [redirect-to (->* {String} {Redirection-Status #:headers (Listof Header)} Response)]
                       [make-digest-auth-header (-> String String String Header)]
                       [request->digest-credentials (-> Request (Option Digest-Credentials))]
                       [make-check-digest-credentials (-> Username*Realm->Digest-HA1 (-> String Digest-Credentials Boolean))]
                       [password->digest-HA1 (-> Username*Realm->Password Username*Realm->Digest-HA1)])

(require/typed/provide web-server/http/bindings
                       [request-headers (-> Request (Listof (Pairof Symbol String)))]
                       [request-bindings (-> Request (Listof (Pairof Symbol (U String Bytes))))]
                       [extract-binding/single (-> Symbol (Listof (Pairof Symbol String)) String)]
                       [extract-bindings (-> Symbol (Listof (Pairof Symbol String)) (Listof String))]
                       [exists-binding? (-> Symbol (Listof (Pairof Symbol String)) Boolean)])
