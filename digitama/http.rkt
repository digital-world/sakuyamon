#lang typed/racket

(require typed/net/url)

(provide (all-defined-out))
(provide (all-from-out typed/net/url))

(define-type Header header)
(define-type Binding binding)
(define-type Request request)
(define-type Response response)

(require/typed/provide web-server/http
                       [headers-assq* (-> Bytes (Listof Header) (Option Header))]
                       [response/xexpr (-> Any [#:code Natural] [#:message Bytes] Response)]
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
                                           [output : (-> Output-Port Void)]}])

(require/typed/provide web-server/configuration/responders
                       [file-response (-> Natural Bytes Path-String Header * Response)]
                       [servlet-loading-responder (-> URL exn Response)]
                       [gen-servlet-not-found (-> Path-String (-> URL Response))]
                       [servlet-error-responder (-> URL exn Response)]
                       [gen-servlet-responder (-> Path-String (-> URL exn Response))]
                       [gen-servlets-refreshed (-> Path-String (-> Response))]
                       [gen-passwords-refreshed (-> Path-String (-> Response))]
                       [gen-authentication-responder (-> Path-String (-> URL Header Response))]
                       [gen-protocol-responder (-> Path-String (-> URL Response))]
                       [gen-file-not-found-responder (-> Path-String (-> Request Response))]
                       [gen-collect-garbage-responder (-> Path-String (-> Response))])

(define response:404 : (-> Path-String Header * Response)
  {lambda [!found.html . headers]
    (apply file-response 404 #"File Not Found" !found.html headers)})

(define response:gc : (-> Response)
  {lambda []
    (define bbyte (current-memory-use))
    (collect-garbage)
    (define mb (/ (- bbyte (current-memory-use)) 1024.0 1024.0))
    (define message (format "~aMB garbage has been collected" (~r mb #:precision '{= 3})))
    (response/xexpr #:code 200 #:message (string->bytes/utf-8 message)
                    `(html (head (title "Garbage Collected"))
                           (body (p ,message))))})

(define response:exn : (-> URL exn Response)
  {lambda [uri exception]
    (servlet-loading-responder uri exception)})
