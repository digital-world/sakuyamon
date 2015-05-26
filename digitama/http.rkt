#lang at-exp typed/racket

@require{digicore.rkt}

(require typed/net/url)
(require typed/net/base64)

(provide (except-out (all-defined-out) response:ddd))
(provide (all-from-out typed/net/url))

(define-type Header header)
(define-type Binding binding)
(define-type Request request)
(define-type Response response)

(define-type Digest-Credentials (Listof (Pairof Symbol String)))
(define-type Username*Realm->Password (-> String String String))
(define-type Username*Realm->Digest-HA1 (-> String String Bytes))

(require/typed racket/base
               [srcloc->string (-> srcloc (Option String))]
               [current-memory-use (->* [] [(Option Custodian)] Nonnegative-Integer)])

(require/typed "posix.rkt"
               [geteuid (-> Natural)]
               [getegid (-> Natural)]
               [fetch_tamer_name (-> Natural (Values Natural Bytes))]
               [fetch_tamer_group (-> Natural (Values Natural Bytes))]
               [syslog (-> Symbol Symbol String Any * Void)])

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

(require/typed/provide file/md5
                       [md5 (->* {(U String Bytes Input-Port)} {Boolean} Bytes)])

(define make-md5-auth-header : (-> String String String Header)
  {lambda [realm private-key0 opaque0]
    (define private-key : Bytes (string->bytes/utf-8 private-key0))
    (define timestamp : Bytes (string->bytes/utf-8 (number->string (current-seconds))))
    (define nonce : Bytes  (md5 (bytes-append timestamp #" " (md5 (bytes-append timestamp #":" private-key)))))
    (define opaque : Bytes (md5 (string->bytes/utf-8 opaque0)))
    (make-header #"WWW-Authenticate"
                 (bytes-append #"Digest realm=\"" (string->bytes/utf-8 realm) #"\""
                               #", qop=\"auth\""
                               #", nonce=\"" nonce #"\""
                               #", opaque=\"" opaque #"\""))})

(define response:ddd : (-> Any Bytes String (Option Path-String) (Listof Header) Response)
  {lambda [code-sexp message desc ddd.html headers]
    (define status-code : Natural (cast (car.eval code-sexp (module->namespace 'racket/base)) Natural))
    (cond [(or (and ddd.html (file-readable? ddd.html) ddd.html)
               (let ([spare.html (build-path (digimon-stone) (format "~a.html" status-code))])
                 (and (file-readable? spare.html) spare.html)))
           => {λ [[ddd.html : Path-String]] (response/full status-code message (current-seconds) TEXT/HTML-MIME-TYPE
                                                           headers (file->bytes-lines ddd.html))}]
          [else (response/xexpr #:code status-code #:message message #:headers headers
                                `(html (head (title ,(bytes->string/utf-8 message))
                                             (link ([rel "stylesheet"] [href "/stone/error.css"])))
                                       (body (div ([class "section"])
                                                  (div ([class "title"]) "Sakuyamon")
                                                  (p "> ((" (a ([href "http://racket-lang.org"]) "uncaught-exception-handler") ") "
                                                     ,(string-replace (format "~a" code-sexp) #px"\\s+" "" #:all? #true) ")"
                                                     (pre "» " ,(number->string status-code)
                                                          " - " ,desc))))))])})

(define response:options : (-> URL (Listof String) Bytes Header * Response)
  {lambda [uri allows terminus . headers]
    (match-define-values {_ id-un} (fetch_tamer_name (geteuid)))
    (match-define-values {_ id-gn} (fetch_tamer_group (getegid)))
    (response/output void #:code 200 #:message #"Metainformation"
                     #:headers (list* (make-header #"Allow" (string->bytes/utf-8 (string-join allows ",")))
                                      (make-header #"Terminus" terminus)
                                      (make-header #"Daemon" id-un)
                                      (make-header #"Realm" id-gn)
                                      headers))})

(define response:gc : (-> Header * Response)
  {lambda headers
    (define ~mb : (-> Integer String) {λ [b] (~r (/ b 1024.0 1024.0) #:precision '{= 3})})
    (define bb : Nonnegative-Integer (current-memory-use))
    (define ab : Nonnegative-Integer (let ([_ (collect-garbage)]) (current-memory-use)))
    (define message : String (format "[~aMB = ~aMB - ~aMB]" (~mb (- bb ab)) (~mb bb) (~mb ab)))
    (syslog 'notice 'realize "collect garbage: ~a" message)
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
    (syslog 'notice 'realize "refresh servlet")
    (response/xexpr #:code 200 #:message #"Servlet Refreshed" #:headers headers
                    `(html (head (title "Refresh Servlet")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> (" (a ([href "http://racket-lang.org"]) "refresh-servlet!") ")"
                                         (pre))))))})

(define response:401 : (-> URL [#:page (Option Path-String)] Header * Response)
  {lambda [url #:page [401.html #false] . headers]
    (syslog 'notice 'unauthorized "~a" (url->string url))
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*))
                  #"Unauthorized" "Authentication Failed!" 401.html headers)})

(define response:403 : (-> [#:page (Option Path-String)] Header * Response)
  {lambda [#:page [403.html #false] . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*)(*))
                  #"Forbidden" "Access Denied!" 403.html headers)})

(define response:404 : (-> [#:page (Option Path-String)] Header * Response)
  {lambda [#:page [404.html #false] . headers]
    (response:ddd '(*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))(+(*)(*)(*)(*)))
                  #"File Not Found" "Resource Not Found!" 404.html headers)})
                  
(define response:418 : (-> [#:page (Option Path-String)] Header * Response)
  {lambda [#:page [418.html #false] . headers]
    (response:ddd '(+(*(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*))
                  #"I am a teapot" "Have a cup of tea?" 418.html headers)})

(define response:exn : (-> (Option URL) exn Bytes [#:page (Option Path-String)] Header * Response)
  {lambda [url x stage #:page [500.html #false] . headers]
    (define tr : (-> String String) {λ [str] (string-replace str (digimon-world) "")})
    (define messages : (Listof Bytes) ((inst call-with-input-string (Listof Bytes)) (exn-message x) port->bytes-lines))
    (syslog 'fatal 'outage "~a: ~a~n~a~n~a" stage (and url (url->string url))
            (string-join (map {λ [[msg : Bytes]] (format "» ~a" msg)} messages) (string #\newline))
            (string-join (filter-map {λ [[stack : (Pairof (Option Symbol) Any)]]
                                       (and (cdr stack)
                                            (let ([srcinfo (srcloc->string (cast (cdr stack) srcloc))])
                                              (and srcinfo (regexp-match? #px"^[^/]" srcinfo)
                                                   (format "»» ~a: ~a~n" (tr srcinfo) (or (car stack) 'λ)))))}
                                     (continuation-mark-set->context (exn-continuation-marks x)))
                         (string #\newline)))
    (cond [(exn:fail:user? x) (response:418)]
          [else (response:ddd '(*(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)))(+(*)(*)(*)(*)(*)))
                              (bytes-append stage #": " (bytes-join messages (string->bytes/utf-8 (string cat#))))
                              "Service Outage!" 500.html headers)])})

(define response:501 : (-> [#:page (Option Path-String)] Header * Response)
  {lambda [#:page [501.html #false] . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*))
                  #"Method Not Implemented" "Method Beyond Capabilities!" 501.html headers)})

(define response:503 : (-> [#:page (Option Path-String)] Header * Response)
  {lambda [#:page [503.html #false] . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*)(*)(*))
                  #"Service Unavailable" "Service Under Construction!" 503.html headers)})
