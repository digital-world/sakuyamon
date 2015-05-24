#lang at-exp racket

@require{digicore.rkt}
@require{posix.rkt}
@require{http.rkt}

(require racket/date)
(require setup/dirs)
 
(require net/tcp-unit)
(require net/ssl-tcp-unit)
(require web-server/private/dispatch-server-sig)

(require web-server/servlet/setup)
(require web-server/dispatchers/dispatch)
(require web-server/dispatchers/filesystem-map)
(require web-server/configuration/namespace)

(require (prefix-in http: web-server/http/request))
(require (prefix-in chain: web-server/dispatchers/dispatch-sequencer))
(require (prefix-in timeout: web-server/dispatchers/dispatch-timeout))
(require (prefix-in pwd: web-server/dispatchers/dispatch-passwords))
(require (prefix-in file: web-server/dispatchers/dispatch-files))
(require (prefix-in servlet: web-server/dispatchers/dispatch-servlets))
(require (prefix-in path: web-server/dispatchers/dispatch-pathprocedure))
(require (prefix-in filter: web-server/dispatchers/dispatch-filter))
(require (prefix-in lift: web-server/dispatchers/dispatch-lift))

(provide sakuyamon-tcp@ sakuyamon-config@)

(define sakuyamon-tcp@ (lazy (cond [(false? (sakuyamon-ssl?)) tcp@]
                                   [else (let ([sakuyamon.crt (build-path (digimon-stone) (format "~a.crt" (current-digimon)))]
                                               [sakuyamon.key (build-path (digimon-stone) (format "~a.key" (current-digimon)))])
                                           (unless (andmap file-exists? (list sakuyamon.crt sakuyamon.key))
                                             (error 'sakuyamon "Please be patient, the age of plaintext transmission is almost over!"))
                                           (make-ssl-tcp@ sakuyamon.crt sakuyamon.key #f #f #f #f #f))])))

(define sakuyamon-config@
  {unit (import)
        (export dispatch-server-config^)

        (define listen-ip #false)
        (define port (cond [(integer? (sakuyamon-port)) (sakuyamon-port)]
                           [(false? (sakuyamon-ssl?)) 80]
                           [else 443]))
        (define max-waiting (sakuyamon-max-waiting))
        (define initial-connection-timeout (sakuyamon-connection-timeout))
        (define read-request http:read-request)

        ;;; These happen before (setuid)
        (define id-ur (getuid))
        (define id-urn (let-values ([{errno id-urn} (fetch_tamer_name id-ur)]) id-urn))
        
        (define host-cache (make-hash)) ; hash has its own semaphor as well as catch-table for ref set! and remove!
        (define path->mime (make-path->mime-type (collection-file-path "mime.types" "web-server" "default-web-root")))

        (define ~path {λ [base uri sub default.rkt] (with-handlers ([exn? {λ _ (raise-user-error 'url->path "Found Escaping `..`!")}])
                                                      (define pas (drop (url-path uri) sub))
                                                      (let travel ([pieces null] [prst (map path/param-path pas)])
                                                        (match prst
                                                          [{cons {or 'up ".."} rst} (travel (cdr pieces) rst)]
                                                          [{cons {or 'same "." ""} rst} (travel pieces rst)]
                                                          [{cons pdir rst} (travel (cons pdir pieces) rst)]
                                                          [_ (let* ([ps (reverse pieces)]
                                                                    [p (simplify-path (apply build-path base ps) #false)])
                                                               (if (and default.rkt (string=? (path/param-path (last pas)) ""))
                                                                   (values (build-path p default.rkt) (reverse (cons default.rkt pieces)))
                                                                   (values p ps)))])))})
        
        (define path->servlet {λ [->path mods] (let ([fns (make-make-servlet-namespace #:to-be-copied-module-specs mods)]
                                                     [tds (sakuyamon-timeout-default-servlet)])
                                                 (servlet:make-cached-url->servlet (filter-url->path #rx"\\.rkt$" (make-url->valid-path ->path))
                                                                                   (make-default-path->servlet #:timeouts-default-servlet tds
                                                                                                               #:make-servlet-namespace fns)))})

        (define ~date (curry ~r #:min-width 2 #:pad-string "0"))
        (define ~host {λ [host] (string->symbol (string-downcase (if (bytes? host) (bytes->string/utf-8 host) host)))})
        (define ~method (compose1 string-upcase bytes->string/utf-8))

        (syslog 'info 'request "header: ~s" '{date user-agent uri client host referer authorization})
        (define syslog-request {λ [req] (let ([now (current-date)]
                                              [a-headers (request-headers req)])
                                          (syslog 'notice 'request "~s"
                                                  (list (format "~a-~a-~a ~a:~a:~a"
                                                                (date-year now) (~date (date-month now))
                                                                (~date (date-day now)) (~date (date-hour now))
                                                                (~date (date-minute now)) (~date (date-second now)))
                                                        (dict-ref a-headers 'user-agent #false)
                                                        (~method (request-method req))
                                                        (url->string (request-uri req))
                                                        (request-client-ip req)
                                                        (dict-ref a-headers 'host #false)
                                                        (dict-ref a-headers 'referer #false)
                                                        (dict-ref a-headers 'authorization #false))))})

        (define dispatch
          {lambda [conn req]
            (with-handlers ([void {λ [?] ((lift:make {λ [req] (response:exn #false ? #"Dispatching")}) conn req)}])
              (define ::1? (string=? (request-client-ip req) "::1")) ; `::1` is a loopback address that happen to affect `host`
              (define host (cond [(url-host (request-uri req)) => ~host]
                                 [(headers-assq* #"Host" (request-headers/raw req)) => (compose1 ~host header-value)]
                                 [else 'none]))
              (define termuni (hash-ref! host-cache host make-hash))
              (define ~:? (let ([pas (url-path (request-uri req))])
                            (with-handlers ([void {λ _ #false}])
                              (match (path/param-path (car pas))
                                [{or "~:"} (and (find-doc-dir) 'racket-docs)]
                                [{var ~:?} (let ([~: (string-split ~:? #px":")])
                                             (and (regexp-match? #"^~.+$" (car ~:))
                                                  (expand-user-path (car ~:))
                                                  ~:))]))))
              ({λ [serve] (serve conn req)}
               (hash-ref! termuni ~:?
                          {λ _ (parameterize ([current-custodian (current-server-custodian)])
                                 (chain:make (timeout:make initial-connection-timeout)
                                             (lift:make {λ [req] (and (syslog-request req) (next-dispatcher))})
                                             (match ~:? ; Why exclusive conditions? branches have already been stored in different caches.
                                               [{list tamer digimon} (cond [(false? (sakuyamon-digimon-terminus?)) (chain:make)]
                                                                           [else (dispatch-digimon tamer digimon ::1?)])] 
                                               [{list tamer} (cond [(false? (sakuyamon-tamer-terminus?)) (chain:make)]
                                                                   [else (dispatch-tamer tamer ::1?)])]
                                               [{or 'racket-docs} (file:make #:url->path {λ [uri] (~path (find-doc-dir) uri 1 #false)}
                                                                             #:path->mime-type path->mime)]
                                               [else (dispatch-main ::1?)])
                                             (lift:make {λ [req] (response:404)})))})))})
        
        (define dispatch-digimon
          {lambda [real-tamer digimon ::1?]
            (define /tamer (build-path (expand-user-path real-tamer) "DigitalWorld" digimon (find-relative-path (digimon-zone) (digimon-tamer))))
            (define /htdocs (build-path /tamer (car (use-compiled-file-paths)) "handbook"))
            (match-define {list realm.rktd robots.txt} (map (curry build-path /tamer) (list ".realm.rktd" "robots.txt")))
            (define-values {<pwd-would-update-automatically> authorize} (pwd:password-file->authorized? realm.rktd))
            (chain:make (lift:make {λ [req] (let ([method (~method (request-method req))]
                                                  [allows '{"GET" "HEAD"}])
                                              (cond [(equal? method "OPTIONS") (response:options (request-uri req) allows #"Per-Digimon")]
                                                    [(member method allows) (next-dispatcher)]
                                                    [else (response:501)]))})
                        (filter:make #px"\\.rktl$" (lift:make {λ [req] (let-values ([{src _} (~path "/" (request-uri req) 1 #false)])
                                                                        (define to (string-replace (substring (path->string src) 1) #px"[/.]" "_"))
                                                                        (define render-depth? (directory-exists? (build-path /htdocs to)))
                                                                        (redirect-to (format "/~a:~a/~a" real-tamer digimon 
                                                                                             (cond [render-depth? (string-append to "/")]
                                                                                                   [else (string-append to ".html")]))))}))
                        (cond [::1? (chain:make)] ; authenticating works after URL rewritting for Scribble.
                              [else (pwd:make (pwd:make-basic-denied?/path authorize)
                                              #:authentication-responder {λ [url header] (response:401 url header)})])
                        (file:make #:path->mime-type path->mime
                                   #:url->path {λ [uri] (~path /htdocs uri 1 #false)})
                        (filter:make (pregexp (string-append "^/(~.*)?:.+?/" (path->string (file-name-from-path robots.txt)) "$"))
                                     (lift:make {λ _ (cond [(file-exists? robots.txt) (file-response 200 #"OK" robots.txt)]
                                                           [else (next-dispatcher)])}))
                        (lift:make {λ _ (cond [(directory-exists? /htdocs) (next-dispatcher)]
                                              [else (response:503)])}))})

        (define dispatch-tamer
          {lambda [real-tamer ::1?]
            (define /zone (build-path (expand-user-path real-tamer) "DigitalWorld" "Kuzuhamon"))
            (define /htdocs (build-path /zone (find-relative-path (digimon-zone) (digimon-terminus))))
            (define 404.html (build-path /zone (find-relative-path (digimon-zone) (digimon-stone)) "404.html"))
            (define realm.rktd (build-path /zone ".realm.rktd"))
            (define url->path {λ [default.rkt uri] (~path /htdocs uri 1 default.rkt)})
            (define-values {refresh-servlet! url->servlet} (path->servlet (curry url->path "default.rkt") null))
            (define-values {lookup-realm lookup-HA1} (realm.rktd->lookups realm.rktd))
            (chain:make (lift:make {λ [req] (let ([method (~method (request-method req))]
                                                  [allows '{"GET" "HEAD" "POST"}])
                                              (cond [(equal? method "OPTIONS") (response:options (request-uri req) allows #"Per-Tamer")]
                                                    [(member method allows) (next-dispatcher)]
                                                    [else (response:501)]))})
                        (filter:make #px"^/~[^:/]*/d-arc/" (cond [(false? ::1?) (lift:make {λ _ (response:403)})]
                                                                [else (filter:make #px"/refresh-servlet$"
                                                                                   (lift:make {λ _ (response:rs refresh-servlet!)}))]))
                        (timeout:make (sakuyamon-timeout-servlet-connection))
                        (cond [::1? (chain:make)]
                              [else (lift:make {λ [req] (match (lookup-realm (url->string (request-uri req)))
                                                          [#false (next-dispatcher)]
                                                          [realm (with-handlers ([exn? {λ [e] (response:exn e)}])
                                                                   (let ([credit (request->digest-credentials req)]
                                                                         [authorize (make-check-digest-credentials lookup-HA1)])
                                                                     (when (and credit (authorize (bytes->string/utf-8 (request-method req)) credit))
                                                                       (next-dispatcher))
                                                                     (define private (symbol->string (gensym (current-digimon))))
                                                                     (define opaque (symbol->string (gensym (current-digimon))))
                                                                     (response:401 (request-uri req) (make-md5-auth-header realm private opaque))))])})])
                        (servlet:make #:responders-servlet-loading (curryr response:exn #"Loading")
                                      #:responders-servlet (curryr response:exn #"Handling")
                                      url->servlet)
                        (file:make #:url->path (curry url->path #false) #:path->mime-type path->mime)
                        (lift:make {λ _ (cond [(directory-exists? /htdocs) (next-dispatcher)]
                                              [(file-exists? 404.html) (file-response 404 #"File Not Found" 404.html)]
                                              [else (response:503)])}))})
        
        (define dispatch-main
          {lambda [::1?]
            (define /htdocs (digimon-terminus))
            (define url->path {λ [default.rkt uri] (~path /htdocs uri 0 default.rkt)})
            (define-values {refresh-servlet! url->servlet} (path->servlet (curry url->path "default.rkt") null))
            (chain:make (lift:make {λ [req] (let ([method (~method (request-method req))]
                                                  [allows '{"GET" "HEAD" "POST"}])
                                              (cond [(equal? method "OPTIONS") (response:options (request-uri req) allows #"Main")]
                                                    [(member method allows) (next-dispatcher)]
                                                    [else (response:501)]))})
                        (filter:make #px"^/d-arc/" (cond [(false? ::1?) (lift:make {λ _ (response:403)})]
                                                         [else (chain:make (path:make "/d-arc/collect-garbage" {λ _ (response:gc)})
                                                                           (path:make "/d-arc/refresh-servlet" {λ _ (response:rs refresh-servlet!)}))]))
                        (timeout:make (sakuyamon-timeout-servlet-connection))
                        (servlet:make #:responders-servlet-loading (curryr response:exn #"Loading")
                                      #:responders-servlet (curryr response:exn #"Handling")
                                      url->servlet)
                        (file:make #:url->path (curry url->path #false) #:path->mime-type path->mime)
                        (cond [(false? (and (sakuyamon-digimon-terminus?) (find-doc-dir))) (chain:make)]
                              [else (lift:make {λ [req] (and (or (regexp-match? #px"^.+?://.+?/~.+?:.+?/" (dict-ref (request-headers req) 'referer ""))
                                                                 (next-dispatcher))
                                                             (let ([~: (string-trim (url->string (request-uri req))
                                                                                    (path->string (find-doc-dir))
                                                                                    #:right? #false)])
                                                               (redirect-to (string-append "/~:" ~:))))})]))})

        (define realm.rktd->lookups
          {lambda [realm.rktd]
            (define timestamp (box #f))
            (define realm-cache (box #f))
            (define update-realms! {λ _ (when (and (file-exists? realm.rktd) (memq 'read (file-or-directory-permissions realm.rktd)))
                                          (let ([cur-mtime (file-or-directory-modify-seconds realm.rktd)])
                                            (when (or (not (unbox timestamp))
                                                      (> cur-mtime (unbox timestamp))
                                                      (not (unbox realm-cache)))
                                              (set-box! realm-cache
                                                        (make-hash (for/list ([group (in-list (cadr (with-input-from-file realm.rktd read)))])
                                                                     (match-define {list-rest realm pattern user.pwds} group)
                                                                     (define ciuser (compose1 string->symbol string-downcase symbol->string car))
                                                                     (define bpwd (compose1 string->bytes/utf-8 cadr))
                                                                     (define userdicts (map {λ [u.p] (cons (ciuser u.p) (bpwd u.p))} user.pwds))
                                                                     (cons realm (cons pattern (make-hasheq userdicts))))))
                                              (set-box! timestamp cur-mtime))))})
            (values {λ [digest-uri] (and (update-realms!)
                                         (unbox realm-cache)
                                         (let/ec deny (and (for ([{realm p.dict} (in-hash (unbox realm-cache))])
                                                             (when (regexp-match? (pregexp (car p.dict)) digest-uri)
                                                               (deny realm)))
                                                           #false)))}
                    {λ [username realm]
                      (define realms (and (update-realms!) (unbox realm-cache)))
                      (or (and realms (let ([p.dict (hash-ref (unbox realm-cache) realm #false)])
                                        (and p.dict (hash-ref (cdr p.dict) (string->symbol (string-downcase username)) #false))))
                          #"denied")})})})

