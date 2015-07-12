#lang at-exp typed/racket

(provide (all-defined-out))

(require typed/net/http-client)
(require typed/net/head)
(require typed/net/base64)
(require typed/file/md5)

@require{digicore.rkt}
@require{typed/web-server/http.rkt}

(define-type Headers (HashTable Symbol String))
(define-type Client-Response (List Positive-Integer String Headers Input-Port))

(define-type TCP-Ports (Pairof Input-Port Output-Port))

(define sakuyamon-agent : (-> String Positive-Integer String * Client-Response)
  (lambda [host port . arglist]
    (define anyauth? : (Parameterof Boolean) (make-parameter #false))
    (define locate? : (Parameterof Boolean) (make-parameter #false))
    (define user:pwd : (Parameterof (Option String)) (make-parameter #false))
    (define go-headers : (Parameterof (Listof String)) (make-parameter null))
    (define http-method : (Parameterof Bytes) (make-parameter #"GET"))
    
    (define retry : (-> String String * Client-Response)
      (lambda [uri . addition]
        (define {remake [arglist : (Listof String)]}
          (remove* (list "--user" "-u" (user:pwd) uri) arglist))
        (apply sakuyamon-agent host port (append addition (remake arglist) (list uri)))))

    (define send/recv : (-> String Client-Response)
      (lambda [uri]
        (call-with-values
         (thunk (http-sendrecv host uri #:port port #:method (http-method) #:headers (reverse (go-headers))))
         (lambda [[status : Bytes] [net-headers : (Listof Bytes)] [/dev/net/stdin : Input-Port]]
           (define-type Field (Pairof (U String Bytes) (U String Bytes)))
           (define parts : (Listof String)
             (cast (regexp-match #px".+?\\s+(\\d+)\\s+(.+)\\s*$" (bytes->string/utf-8 status)) (Listof String)))
           (list (cast (string->number (cadr parts)) Positive-Integer)
                 (string-join (string-split (list-ref parts 2) (string cat#)) (string #\newline))
                 (for/hash : Headers ([field : Field (in-list (apply append (map extract-all-fields net-headers)))])
                   (values (string->symbol (string-downcase (bytes->string/utf-8 (cast (car field) Bytes))))
                           (bytes->string/utf-8 (cast (cdr field) Bytes))))
                 /dev/net/stdin)))))

    (define authorize : (-> String Headers Client-Response)
      (lambda [uri headers]
        (define WWW-Authenticate ((inst hash-ref Symbol String Nothing) headers 'www-authenticate))
        (define px.k=v #px#"(\\w+)=\"(.+?)\"")
        (cond [(regexp-match #px"^Basic" WWW-Authenticate)
               (retry uri "--header" (format "Authorization: Basic ~a"
                                             (base64-encode (string->bytes/utf-8 (cast (user:pwd) String)))))]
              [(regexp-match #px"^Digest" WWW-Authenticate)
               (with-handlers ([exn? raise])
                 (define bindings : (HashTable Symbol Bytes)
                   (for/hash : (HashTable Symbol Bytes) ([k-v (in-list (regexp-match* px.k=v WWW-Authenticate))])
                     (define kv : (Listof Bytes) (cast (regexp-match px.k=v k-v) (Listof Bytes)))
                     (values (string->symbol (string-downcase (bytes->string/utf-8 (cadr kv))))
                             (caddr kv))))
                 (match-define {list _ user pwd}
                   (cast (regexp-match #px"^([^:]+)?:(.+)?$" (cast (user:pwd) String)) (Listof String)))
                 (define nonce-count (~a "1" #:max-width 8 #:min-width 8 #:align 'right #:pad-string "0"))
                 (define realm ((inst hash-ref Symbol Bytes Nothing) bindings 'realm))
                 (define qop ((inst hash-ref Symbol Bytes Nothing) bindings 'qop))
                 (define nonce ((inst hash-ref Symbol Bytes Nothing) bindings 'nonce)) 
                 (define timestamp (number->string (current-seconds)))
                 (define cnonce (md5 (format "~a ~a" timestamp (md5 timestamp))))
                 (retry uri "--header" (format (string-append "Authorization: Digest nc=~a" 
                                                              ", realm=\"~a\", username=\"~a\""
                                                              ", nonce=\"~a\", cnonce=\"~a\""
                                                              ", qop=\"~a\", uri=\"~a\""
                                                              ", response=\"~a\"")
                                               nonce-count realm user nonce cnonce qop uri
                                               (md5 (format "~a:~a:~a:~a:~a:~a"
                                                            ((password->digest-HA1 ((inst const String) pwd))
                                                             user (bytes->string/utf-8 realm))
                                                            nonce nonce-count cnonce qop
                                                            (md5 (format "~a:~a" (http-method) uri)))))))]
              [else (raise 500)])))
    
    ((cast parse-command-line (-> String (Listof String) Help-Table (-> Any String Client-Response)
                                  (List String) (-> String Any) Client-Response))
     "sakuyamon-curl"
     arglist
     `([usage-help ,(format "Transfer URL: a cURL-like tool for taming only.~n")]
       [once-each
        [{"--anyauth"} ,(lambda [[_ : String]] (anyauth? #true))
         {"Detect authentication method."}]
        [{"--location" "-L"} ,(lambda [[_ : String]] (locate? #true))
         {"Follow redirects."}]
        [{"--get" "-G"} ,(lambda [[_ : String]] (http-method #"GET"))
         {"Send with HTTP GET."}]
        [{"--head" "-I"} ,(lambda [[_ : String]] (http-method #"HEAD"))
         {"Show document info only."}]
        [{"--user" "-u"} ,(lambda [[_ : String] [u:p : String]] (user:pwd u:p))
         {"Server user and password." "USER:PASSWORD"}]
        [{"--request" "-X"} ,(lambda [[_ : String] [cmd : String]] (http-method (string->bytes/utf-8 (string-upcase cmd))))
         {"Specify request command to use." "COMMAND"}]]
       [multi
        [{"--header" "-H"} ,(lambda [[_ : String] [line : String]] (go-headers (cons line (go-headers))))
         {"Custom header to pass to server." "LINE"}]])
     (lambda [[! : Any] [uri : String]]
       (define status (send/recv uri))
       (match-define {list code _ headers _} status)
       (with-handlers ([void (const status)])
         (cond [(and (locate?) (member code '{301 302 307 308}) (member (http-method) '{#"GET" #"HEAD"}))
                (retry ((inst hash-ref Symbol String Nothing) headers 'location))]
               [(and (user:pwd) (anyauth?) (eq? code 401))
                (authorize uri headers)]
               [else status])))
     (list "url")
     (lambda [[-h : String]]
       ;;; No one will keep calling `help` in practice,
       ;;; So it just raises error here in order to avoiding declaring two return types.
       (raise-user-error (string-replace -h #px"  -- : .+?-h --'\\s*" ""))))))
