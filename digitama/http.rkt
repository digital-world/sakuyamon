#lang at-exp typed/racket

@require{digicore.rkt}

(require typed/net/url)

(provide (all-defined-out))
(provide (all-from-out typed/net/url))

(define-type Header header)
(define-type Binding binding)
(define-type Request request)
(define-type Response response)

(require/typed racket/base
               [srcloc->string (-> srcloc (Option String))])

(require/typed/provide web-server/http
                       [headers-assq* (-> Bytes (Listof Header) (Option Header))]
                       [response/xexpr (-> Any [#:code Natural] [#:message Bytes] [#:headers (Listof Header)] Response)]
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

(define response:403 : (-> Header * Response)
  {lambda headers
    (response/xexpr #:code 403 #:message #"Access Denied" #:headers headers
                    `(html (head (title "Access Denied")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> ((" (a ([href "http://racket-lang.org"]) "uncaught-exception-handler") ") "
                                         "(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*)(*))" ")"
                                         (pre "» 403 - Access Denied!"))))))})

(define response:404 : (-> Header * Response)
  {lambda headers
    (response/xexpr #:code 404 #:message #"File Not Found" #:headers headers
                    `(html (head (title "File Not Found")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> ((" (a ([href "http://racket-lang.org"]) "uncaught-exception-handler") ") "
                                         "(*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))(+(*)(*)(*)(*)))" ")"
                                         (pre "» 404 - Resource Not Found!"))))))})

(define response:418 : (-> Header * Response)
  {lambda headers
    (response/xexpr #:code 418 #:message #"I am a teapot" #:headers headers
                    `(html (head (title "I am a teapot")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> ((" (a ([href "http://racket-lang.org"]) "uncaught-exception-handler") ") "
                                         "(+(*(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*))" ")"
                                         (pre "» 418 - Have a cup of tea?"))))))})

(define response:gc : (-> Header * Response)
  {lambda headers
    (define ~mb : (-> Integer String) {λ [b] (~r (/ b 1024.0 1024.0) #:precision '{= 3})})
    (define bb : Nonnegative-Integer (current-memory-use))
    (define ab : Nonnegative-Integer (let ([_ (collect-garbage)]) (current-memory-use)))
    (define message : String (format "[~aMB = ~aMB - ~aMB]" (~mb (- bb ab)) (~mb bb) (~mb ab)))
    (response/xexpr #:code 200 #:message (string->bytes/utf-8 message) #:headers headers
                    `(html (head (title "Collect Garbage")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> (" (a ([href "http://racket-lang.org"]) "collect-garbage") ")"
                                         (pre "» " ,message))))))})

(define response:rs : (-> (-> Void) Header * Response)
  {lambda [refresh-servlet! . headers]
    (refresh-servlet!)
    (response/xexpr #:code 200 #:message #"Servlet Refreshed" #:headers headers
                    `(html (head (title "Refresh Servlet")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> (" (a ([href "http://racket-lang.org"]) "refresh-servlet!") ")"
                                         (pre))))))})

(define response:exn : (-> (Option URL) exn Bytes Header * Response)
  {lambda [url x stage . headers]
    (cond [(exn:fail:user? x) (response:418)]
          [(exn? x) (let ([tr : (-> String String) {λ [str] (string-replace str (digimon-world) "")}])
                      (response/xexpr #:code 500 #:headers headers
                                      #:message (let ([msgs ((inst call-with-input-string (Listof Bytes)) (exn-message x) port->bytes-lines)])
                                                  (bytes-append stage #": " (bytes-join msgs (string->bytes/utf-8 (string cat#)))))
                                      `(html (head (title ,(format "Uncaught Exception when ~a" stage))
                                                   (link ([rel "stylesheet"] [href "/stone/error.css"])))
                                             (body (div ([class "section"])
                                                        (div ([class "title"]) "Sakuyamon")
                                                        (p "> ((" (a ([href "http://racket-lang.org"]) "uncaught-exception-handler") ") "
                                                           "(*(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)))(+(*)(*)(*)(*)(*)))" ")"
                                                           (pre ,@(map {λ [v] (format "» ~a~n" v)}
                                                                       (list (if url (tr (url->string url)) "#false")
                                                                             (object-name x)
                                                                             (tr (exn-message x))))
                                                                ,@(filter-map {λ [[stack : (Pairof (Option Symbol) Any)]]
                                                                                (and (cdr stack)
                                                                                     (let ([srcinfo (srcloc->string (cast (cdr stack) srcloc))])
                                                                                       (and srcinfo (regexp-match? #px"^[^/]" srcinfo)
                                                                                            (format "»»» ~a: ~a~n" (tr srcinfo) (or (car stack) 'λ)))))}
                                                                              (continuation-mark-set->context (exn-continuation-marks x))))))))))]
          [else (raise x)])})
