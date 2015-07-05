#lang at-exp typed/racket

@require{digicore.rkt}
@require{daemon.rkt}

@require{typed/web-server/http.rkt}
@require{typed/web-server/configuration/responders.rkt}

(require typed/net/url)
(require typed/file/md5)

(provide (except-out (all-defined-out) response:ddd))
(provide (all-from-out typed/net/url))

(provide (all-from-out "typed/web-server/http.rkt"
                       "typed/web-server/configuration/responders.rkt"))

(require/typed racket/base
               [srcloc->string (-> srcloc (Option String))]
               [current-memory-use (->* [] [(Option Custodian)] Nonnegative-Integer)])

(define make-md5-auth-header : (-> String String String Header)
  (lambda [realm private-key0 opaque0]
    (define private-key : Bytes (string->bytes/utf-8 private-key0))
    (define timestamp : Bytes (string->bytes/utf-8 (number->string (current-seconds))))
    (define nonce : Bytes  (md5 (bytes-append timestamp #" " (md5 (bytes-append timestamp #":" private-key)))))
    (define opaque : Bytes (md5 (string->bytes/utf-8 opaque0)))
    (header #"WWW-Authenticate"
            (bytes-append #"Digest realm=\"" (string->bytes/utf-8 realm) #"\""
                          #", qop=\"auth\""
                          #", nonce=\"" nonce #"\""
                          #", opaque=\"" opaque #"\""))))

(define response:ddd : (-> Any Bytes String (Option Path-String) (Listof Header) Response)
  (lambda [code-sexp message desc ddd.html headers]
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
                                                          " - " ,desc))))))])))

(define response:options : (-> URL (Listof String) Bytes Header * Response)
  (lambda [uri allows terminus . headers]
    (define id-un (fetch_tamer_name (geteuid)))
    (define id-gn (fetch_tamer_group (getegid)))
    (response/output void #:code 200 #:message #"Metainformation"
                     #:headers (list* (header #"Allow" (string->bytes/utf-8 (string-join allows ",")))
                                      (header #"Terminus" terminus)
                                      (header #"Daemon" id-un)
                                      (header #"Realm" id-gn)
                                      headers))))

(define response:gc : (-> Header * Response)
  (lambda headers
    (define ~mb : (-> Integer String) (lambda [b] (~r (/ b 1024.0 1024.0) #:precision '{= 3})))
    (define bb : Nonnegative-Integer (current-memory-use))
    (define ab : Nonnegative-Integer (let ([_ (collect-garbage)]) (current-memory-use)))
    (define message : String (format "[~aMB = ~aMB - ~aMB]" (~mb (- bb ab)) (~mb bb) (~mb ab)))
    (rsyslog 'notice 'realize (~a "collect garbage: " message))
    (response/xexpr #:code 200 #:message (string->bytes/utf-8 message) #:headers headers
                    `(html (head (title "Collect Garbage")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> (" (a ([href "http://racket-lang.org"]) "collect-garbage") ")"
                                         (pre "» " ,message))))))))

(define response:rs : (-> (-> Void) Header * Response)
  (lambda [refresh-servlet! . headers]
    (refresh-servlet!)
    (rsyslog 'notice 'realize "refresh servlet")
    (response/xexpr #:code 200 #:message #"Servlet Refreshed" #:headers headers
                    `(html (head (title "Refresh Servlet")
                                 (link ([rel "stylesheet"] [href "/stone/error.css"])))
                           (body (div ([class "section"])
                                      (div ([class "title"]) "Sakuyamon")
                                      (p "> (" (a ([href "http://racket-lang.org"]) "refresh-servlet!") ")"
                                         (pre))))))))

(define response:401 : (-> URL [#:page (Option Path-String)] Header * Response)
  (lambda [url #:page [401.html #false] . headers]
    (rsyslog 'notice 'unauthorized (url->string url))
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*))
                  #"Unauthorized" "Authentication Failed!" 401.html headers)))

(define response:403 : (-> [#:page (Option Path-String)] Header * Response)
  (lambda [#:page [403.html #false] . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*)(*))
                  #"Forbidden" "Access Denied!" 403.html headers)))

(define response:404 : (-> [#:page (Option Path-String)] Header * Response)
  (lambda [#:page [404.html #false] . headers]
    (response:ddd '(*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))(+(*)(*)(*)(*)))
                  #"File Not Found" "Resource Not Found!" 404.html headers)))
                  
(define response:418 : (-> [#:page (Option Path-String)] Header * Response)
  (lambda [#:page [418.html #false] . headers]
    (response:ddd '(+(*(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)))(*)(*))
                  #"I am a teapot" "Have a cup of tea?" 418.html headers)))

(define response:exn : (-> (Option URL) exn Bytes [#:page (Option Path-String)] Header * Response)
  (lambda [url x stage #:page [500.html #false] . headers]
    (define messages : (Listof Bytes) ((inst call-with-input-string (Listof Bytes)) (exn-message x) port->bytes-lines))
    (rsyslog 'fatal 'outage
             (~a stage ": "
                 (and url (url->string url))
                 #\newline
                 (string-join (map {λ [[msg : Bytes]] (format "» ~a" msg)} messages) (string #\newline))
                 #\newline
                 (string-join (filter-map (lambda [[stack : (Pairof (Option Symbol) Any)]]
                                            (and (cdr stack)
                                                 (let ([srcinfo (srcloc->string (cast (cdr stack) srcloc))])
                                                   (and srcinfo (regexp-match? #px"^[^/]" srcinfo)
                                                        (format "»» ~a: ~a~n" srcinfo (or (car stack) 'λ))))))
                                          (continuation-mark-set->context (exn-continuation-marks x)))
                              (string #\newline))))
    (cond [(exn:fail:user? x) (response:418)]
          [else (response:ddd '(*(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)))(+(*)(*)(*)(*)(*)))
                              (bytes-append stage #": " (bytes-join messages (string->bytes/utf-8 (string cat#))))
                              "Service Outage!" 500.html headers)])))

(define response:501 : (-> [#:page (Option Path-String)] Header * Response)
  (lambda [#:page [501.html #false] . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*))
                  #"Method Not Implemented" "Method Beyond Capabilities!" 501.html headers)))

(define response:503 : (-> [#:page (Option Path-String)] Header * Response)
  (lambda [#:page [503.html #false] . headers]
    (response:ddd '(+(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))(+(*)(*)(*)(*)(*)))(*)(*)(*))
                  #"Service Unavailable" "Service Under Construction!" 503.html headers)))
