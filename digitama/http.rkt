#lang at-exp typed/racket

@require{digicore.rkt}

(require typed/net/url)

(provide (except-out (all-defined-out) response:ddd))
(provide (all-from-out typed/net/url))

(define-type Header header)
(define-type Binding binding)
(define-type Request request)
(define-type Response response)

(define-type Digest-Credentials (Listof (Pairof Symbol Symbol)))
(define-type Username*Realm->Password (-> String String String))
(define-type Username*Realm->Digest-HA1 (-> String String Bytes))

(require/typed racket/base
               [srcloc->string (-> srcloc (Option String))]
               [current-memory-use (->* [] [(Option Custodian)] Nonnegative-Integer)])

(require/typed/provide web-server/http
                       [#:opaque Redirection-Status redirection-status?]
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
                       [response/xexpr (-> Any [#:code Natural] [#:message Bytes] [#:headers (Listof Header)] Response)]
                       [redirect-to (->* {String} {Redirection-Status #:headers (Listof Header)} Response)]
                       [make-digest-auth-header (-> String String String Header)]
                       [request->digest-credentials (-> Request (Option Digest-Credentials))]
                       [make-check-digest-credentials (-> Username*Realm->Digest-HA1 (-> String Digest-Credentials Boolean))]
                       [password->digest-HA1 (-> Username*Realm->Password Username*Realm->Digest-HA1)])

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

(require/typed/provide web-server/private/web-server-structs
                       [current-server-custodian (Parameterof Custodian)]
                       [make-servlet-custodian (-> Custodian)])

(require/typed/provide web-server/private/mime-types
                       [read-mime-types (-> Path-String (HashTable Symbol Bytes))]
                       [make-path->mime-type (-> Path-String (-> Path (Option Bytes)))])

(define response:ddd : (-> Any Bytes String (Listof Header) Response)
  {lambda [code-sexp message desc headers]
    (define status-code : Natural (cast (car.eval code-sexp (module->namespace 'racket/base)) Natural))
    (response/xexpr #:code status-code #:message message #:headers headers
                    `(html (head (title ,(bytes->string/utf-8 message))
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> ((" (a ([href "http://racket-lang.org"]) "uncaught-exception-handler") ") "
                                         ,(string-replace (format "~a" code-sexp) #px"\\s+" "" #:all? #true) ")"
                                         (pre "» " ,(number->string status-code)
                                              " - " ,desc))))))})

(define response:401 : (-> URL Header * Response)
  {lambda [url . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*))
                  #"Unauthorized" "Authentication Failed!" headers)})

(define response:403 : (-> Header * Response)
  {lambda headers
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*)(*))
                  #"Forbidden" "Access Denied!" headers)})

(define response:404 : (-> Header * Response)
  {lambda headers
    (response:ddd '(*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))(+(*)(*)(*)(*)))
                  #"File Not Found" "Resource Not Found!" headers)})
                  
(define response:418 : (-> Header * Response)
  {lambda headers
    (response:ddd '(+(*(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*))
                  #"I am a teapot" "Have a cup of tea?" headers)})

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
    (cond [(false? (exn? x)) (raise x)]
          [(exn:fail:user? x) (response:418)]
          [else (let ([tr : (-> String String) {λ [str] (string-replace str (digimon-world) "")}])
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
                                                                          (continuation-mark-set->context (exn-continuation-marks x))))))))))])})
