#lang at-exp typed/racket

(provide dispatch)

@require{digicore.rkt}
@require[(submod "posix.rkt" typed/ffi)]

@require{http.rkt}

@require{unstable-typed/web-server/private.rkt}
@require{unstable-typed/web-server/dispatchers.rkt}

(require typed/web-server/configuration/responders)

(require/typed setup/dirs
               [find-doc-dir (-> (Option Path))])

(define-type Terminus (U 'racket-docs (Listof String) False))
(define-type Dispatchers (HashTable Terminus Dispatcher))

(define initial-connection-timeout : Integer (sakuyamon-connection-timeout))

(define /htdocs : Path-String (digimon-terminus))
(define zone-stone : String (~a (find-relative-path (digimon-zone) (digimon-stone))))
(define zone-tamer : String (~a (find-relative-path (digimon-zone) (digimon-tamer))))
(define zone-terminus : String (~a (find-relative-path (digimon-zone) (digimon-terminus))))

(define host-cache : (HashTable Symbol Dispatchers) (make-hash))
(define path->mime : (-> Path (Option Bytes)) (make-path->mime-type (collection-file-path "mime.types" "web-server" "default-web-root")))

(define ~path : (-> Path-String URL Index (Option Path-String) (Values Path (Listof Path-Piece)))
  (lambda [base uri sub default.rkt]
    (with-handlers ([exn? (λ _ (raise-user-error 'url->path "Found Escaping `..`!"))])
      (define pas (drop (url-path uri) sub))
      (let travel ([pieces : (Listof Path-String) null] [prst (map path/param-path pas)])
        (match prst
          [(cons (or 'up "..") rst) (travel (cdr pieces) rst)]
          [(cons (or 'same "." "") rst) (travel pieces rst)]
          [(cons (? string? pdir) rst) (travel (cons pdir pieces) rst)]
          [_ (let* ([ps (reverse pieces)]
                    [p (simplify-path (apply build-path base ps) #false)])
               (if (and default.rkt (equal? (path/param-path (last pas)) ""))
                   (values (build-path p default.rkt) (reverse (cons default.rkt pieces)))
                   (values p ps)))])))))

(define path->servlet : (-> URL->Path (Listof Module-Path) (Values (-> Void) URL->Servlet))
  (lambda [->path mods]
    (define fmsn : Make-Servlet-Namespace (make-make-servlet-namespace #:to-be-copied-module-specs mods))
    (define tds : Integer (sakuyamon-timeout-default-servlet))
    (make-cached-url->servlet (filter-url->path #rx"\\.rkt$" (make-url->valid-path ->path))
                              (make-default-path->servlet #:make-servlet-namespace fmsn
                                                          #:timeouts-default-servlet tds))))

(define ~host : (-> (U String Bytes) Symbol)
  (lambda [host]
    (string->symbol (string-downcase (if (bytes? host) (bytes->string/utf-8 host) host)))))

(define ~method : (-> Bytes String)
  (lambda [method]
    (string-upcase (bytes->string/utf-8 method))))

(define syslog-request : (-> Request Void)
  (lambda [req]
    (rsyslog 'notice 'request
             (~s (make-hash (list* (cons 'logging-timestamp (current-inexact-milliseconds))
                                   (cons 'method (~method (request-method req)))
                                   (cons 'uri (url->string (request-uri req)))
                                   (cons 'client (request-client-ip req))
                                   (for/list : (Listof (Pairof Symbol Any)) ([head (in-list (request-headers/raw req))])
                                     (cons (string->symbol (string-downcase (bytes->string/utf-8 (header-field head))))
                                           (header-value head)))))))))

(define dispatch : (-> Connection Request Void)
  (lambda [conn req]
    (with-handlers ([exn? (λ [[? : exn]] ((lift:make (λ [req] (response:exn #false ? #"Dispatching"))) conn req))])
      (define ::1? : Boolean (string=? (request-client-ip req) "::1")) ; `::1` is a loopback address that happen to affect `host`
      (define host : Symbol (cond [(url-host (request-uri req)) => ~host]
                                  [(headers-assq* #"Host" (request-headers/raw req)) => (compose1 ~host header-value)]
                                  [else 'none]))
      (define termuni : Dispatchers (hash-ref! host-cache host (λ [] ((inst make-hash Terminus Dispatcher)))))
      (define ~:? : Terminus
        (let ([pas : (Listof Path/Param) (url-path (request-uri req))])
          (match (path/param-path (car pas))
            [(? symbol?) #false]
            [(or "~:") (and (find-doc-dir) 'racket-docs)]
            [(? string? ~:?) (let ([~: (string-split ~:? #px":")])
                               (and (regexp-match? #"^~.+$" (car ~:))
                                    (expand-user-path (car ~:))
                                    ~:))])))
      (define serve : Dispatcher
        (hash-ref! termuni ~:?
                   (λ _ (parameterize ([current-custodian (current-server-custodian)])
                          (define digimon-terminus? : Boolean (sakuyamon-digimon-terminus?))
                          (define tamer-terminus? : Boolean (sakuyamon-tamer-terminus?))
                          (define racket-doc-dir : (Option Path) (find-doc-dir))
                          (sequencer:make (timeout:make initial-connection-timeout)
                                          (lift:make (λ [req] (response:rs (λ [] (syslog-request req) (next-dispatcher)))))
                                          (match ~:? ; Why exclusive conditions? branches have already been stored in different caches.
                                            [(list tamer digimon)
                                             (cond [(false? digimon-terminus?) (sequencer:make)]
                                                   [else (dispatch-digimon tamer digimon ::1?)])] 
                                            [(list tamer)
                                             (cond [(false? tamer-terminus?) (sequencer:make)]
                                                   [else (dispatch-tamer tamer ::1?)])]
                                            [(or 'racket-docs)
                                             (cond [(false? racket-doc-dir) (sequencer:make)]
                                                   [else (file:make #:url->path (λ [[u : URL]] (~path racket-doc-dir u 1 #false))
                                                                    #:path->mime-type path->mime)])]
                                            [else (dispatch-main ::1?)])
                                          (lift:make (λ [req] (response:404))))))))
      (serve conn req))))

(define dispatch-digimon : (-> String String Boolean Dispatcher)
  (lambda [real-tamer digimon ::1?]
    (define /tamer : Path (build-path (expand-user-path real-tamer) "DigitalWorld" digimon zone-tamer))
    (define /htdocs : Path (build-path /tamer (car (use-compiled-file-paths)) "handbook"))
    (define robots.txt : String "robots.txt")
    (define /robots.txt : Path (build-path /tamer robots.txt))
    (define px.robots.txt : PRegexp (pregexp (string-append "^/(~.*)?:.+?/" robots.txt "$")))
    (define-values (<pwd-would-update-automatically> authorize) (password-file->authorized? (build-path /tamer ".realm.rktd")))
    (sequencer:make (lift:make (λ [req] (let ([method (~method (request-method req))]
                                              [allows '("GET" "HEAD")])
                                          (cond [(equal? method "OPTIONS") (response:options (request-uri req) allows #"Per-Digimon")]
                                                [(member method allows) (response:rs next-dispatcher)]
                                                [else (response:501)]))))
                    (filter:make #px"\\.rkt$" (lift:make (λ [req] (let-values ([(src _) (~path "/" (request-uri req) 1 #false)])
                                                                    (define to (string-replace (substring (path->string src) 1) #px"[/.]" "_"))
                                                                    (define render-depth? (directory-exists? (build-path /htdocs to)))
                                                                    (redirect-to (format "/~a:~a/~a" real-tamer digimon 
                                                                                         (cond [render-depth? (string-append to "/")]
                                                                                               [else (string-append to ".html")])))))))
                    (cond [::1? (sequencer:make)] ; authenticating works after URL rewritting for Scribble.
                          [else (password:make (make-basic-denied?/path authorize)
                                               #:authentication-responder (λ [[u : URL] [h : Header]] (response:401 u h)))])
                    (file:make #:path->mime-type path->mime
                               #:url->path (λ [[u : URL]] (~path /htdocs u 1 #false)))
                    (cond [(false? (file-readable? /robots.txt)) (sequencer:make)]
                          [else (filter:make px.robots.txt (lift:make (λ _ (file-response 200 #"OK" /robots.txt))))])
                    (cond [(directory-exists? /htdocs) (sequencer:make)]
                          [else (lift:make (λ _ (response:503)))]))))
  
(define dispatch-tamer : (-> String Boolean Dispatcher)
  (lambda [real-tamer ::1?]
    (define /zone : Path (build-path (expand-user-path real-tamer) "DigitalWorld" "Kuzuhamon"))
    (define /htdocs : Path (build-path /zone zone-terminus))
    (define page : (-> Any Path) (λ [code] (build-path /zone zone-stone (format "~a.html" code))))
    (define realm.rktd : Path (build-path /zone ".realm.rktd"))
    (define-values (refresh-servlet! url->servlet) (path->servlet (λ [[u : URL]] (~path /htdocs u 1 "default.rkt")) null))
    (define-values (lookup-realm lookup-HA1) (realm.rktd->lookups realm.rktd))
    (sequencer:make (lift:make (λ [req] (let ([method (~method (request-method req))]
                                              [allows '("GET" "HEAD" "POST")])
                                          (cond [(equal? method "OPTIONS") (response:options (request-uri req) allows #"Per-Tamer")]
                                                [(member method allows) (response:rs next-dispatcher)]
                                                [else (response:501 #:page (page 501))]))))
                    (filter:make #px"^/~[^:/]*/d-arc/" (cond [(false? ::1?) (lift:make (λ _ (response:403 #:page (page 403))))]
                                                             [else (filter:make #px"/refresh-servlet$"
                                                                                (lift:make (λ _ (response:rs refresh-servlet!))))]))
                    (timeout:make (sakuyamon-timeout-servlet-connection))
                    (cond [::1? (sequencer:make)]
                          [else (lift:make (λ [req] (match (lookup-realm (url->string (request-uri req)))
                                                      [#false (response:rs next-dispatcher)]
                                                      [(? string? realm)
                                                       (with-handlers ([exn? (λ [[e : exn]] (response:exn (request-uri req) e #"Authorizing"))])
                                                         (let ([credit (request->digest-credentials req)]
                                                               [authorize (make-check-digest-credentials lookup-HA1)])
                                                           (when (and credit (authorize (bytes->string/utf-8 (request-method req)) credit))
                                                             (next-dispatcher))
                                                           (define private (symbol->string (gensym (current-digimon))))
                                                           (define opaque (symbol->string (gensym (current-digimon))))
                                                           (response:401 #:page (page 401) (request-uri req)
                                                                         (make-digest-auth-header realm private opaque))))])))])
                    (servlet:make #:responders-servlet-loading (λ [[u : URL] [e : exn]] (response:exn u e #:page (page 500) #"Loading"))
                                  #:responders-servlet (λ [[u : URL] [e : exn]] (response:exn u e #:page (page 500) #"Handling"))
                                  url->servlet)
                    (file:make #:path->mime-type path->mime
                               #:url->path (λ [[u : URL]] (~path /htdocs u 1 #false)))
                    (cond [(directory-exists? /htdocs) (lift:make (λ _ (response:404 #:page (page 404))))]
                          [else (lift:make (λ _ (response:503 #:page (page 503))))]))))
  
(define dispatch-main : (-> Boolean Dispatcher)
  (lambda [::1?]
    (define-values (refresh-servlet! url->servlet) (path->servlet (λ [[u : URL]] (~path /htdocs u 0 "default.rkt")) null))
    (sequencer:make (lift:make (λ [req] (let ([method (~method (request-method req))]
                                              [allows '("GET" "HEAD" "POST")])
                                          (cond [(equal? method "OPTIONS") (response:options (request-uri req) allows #"Main")]
                                                [(member method allows) (response:rs next-dispatcher)]
                                                [else (response:501)]))))
                    (filter:make #px"^/d-arc/" (cond [(false? ::1?) (lift:make (λ _ (response:403)))]
                                                     [else (sequencer:make (pathprocedure:make "/d-arc/collect-garbage"
                                                                                               (λ [req] (response:gc)))
                                                                           (pathprocedure:make "/d-arc/refresh-servlet"
                                                                                               (λ [req] (response:rs refresh-servlet!))))]))
                    (timeout:make (sakuyamon-timeout-servlet-connection))
                    (servlet:make #:responders-servlet-loading (λ [[u : URL] [e : exn]] (response:exn u e #"Loading"))
                                  #:responders-servlet (λ [[u : URL] [e : exn]] (response:exn u e #"Handling"))
                                  url->servlet)
                    (file:make #:url->path (λ [[u : URL]] (~path /htdocs u 0 #false))
                               #:path->mime-type path->mime))))
  
(define realm.rktd->lookups : (-> Path-String (Values (-> String (Option String)) Username*Realm->Digest-HA1))
  (lambda [realm.rktd]
    (define-type UserDB (HashTable String Bytes))
    (define-type RealmDB (HashTable String (Pairof Regexp UserDB)))
    (define realm-cache : (Vector Natural RealmDB) (vector 0 (make-hash)))
    (define (update-realms!)
      (when (file-readable? realm.rktd)
        (let ([cur-mtime (file-or-directory-modify-seconds realm.rktd)])
          (when (> cur-mtime (vector-ref realm-cache 0))
            (define /etc/passwd : (Listof HTTP-Password)
              (cadr (cast (with-input-from-file realm.rktd read) (List 'quote (Listof HTTP-Password)))))
            (define realms : RealmDB (vector-ref realm-cache 1))
            (define new-realms : (Listof String)
              (for/list : (Listof String) ([record : HTTP-Password (in-list /etc/passwd)])
                (match-define (list-rest realm pattern user.pwds) record)
                (define users : UserDB (if (hash-has-key? realms realm) (cdr (hash-ref realms realm)) (make-hash)))
                (define new-users : (Listof String)
                  (for/list : (Listof String) ([u.p (in-list user.pwds)])
                    (define username : String (string-downcase (symbol->string (car u.p))))
                    (hash-set! users username (string->bytes/utf-8 (cadr u.p)))
                    username))
                (for ([old-user : String (in-list (hash-keys users))])
                  (unless (member old-user new-users) (hash-remove! users old-user)))
                (hash-set! realms realm (cons (pregexp pattern) users))
                realm))
            (for ([old-realm : String (in-list (hash-keys realms))])
              (unless (member old-realm new-realms) (hash-remove! realms old-realm)))
            (vector-set! realm-cache 0 cur-mtime)))))
    (values (lambda [[digest-uri : String]] ; lookup realm
              (update-realms!)
              (let/ec deny : String
                (for ([(realm p.dict) (in-hash (vector-ref realm-cache 1))])
                  (when (regexp-match? (car p.dict) digest-uri)
                    (deny realm)))
                #false))
            (lambda [[username : String] [realm : String]] ; lookup SHA1
              (update-realms!)
              (or (let ([pxurl.dict (hash-ref (vector-ref realm-cache 1) realm #false)])
                    (and pxurl.dict (hash-ref (cdr pxurl.dict) (string-downcase username) #false)))
                  #"denied")))))
