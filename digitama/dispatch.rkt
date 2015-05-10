#lang at-exp racket

@require{digicore.rkt}
@require{http.rkt}

(require net/tcp-unit)
(require net/ssl-tcp-unit)
(require web-server/private/dispatch-server-sig)

(require web-server/servlet/setup)
(require web-server/dispatchers/filesystem-map)
(require web-server/configuration/namespace)

(require (prefix-in http: web-server/http/request))
(require (prefix-in chain: web-server/dispatchers/dispatch-sequencer))
(require (prefix-in timeout: web-server/dispatchers/dispatch-timeout))
(require (prefix-in pwd: web-server/dispatchers/dispatch-passwords))
(require (prefix-in file: web-server/dispatchers/dispatch-files))
(require (prefix-in servlet: web-server/dispatchers/dispatch-servlets))
(require (prefix-in path: web-server/dispatchers/dispatch-pathprocedure))
(require (prefix-in log: web-server/dispatchers/dispatch-log))
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
        
        (define dispatch-cache (make-hash)) ; hash has its own semaphor as well as catch-table for ref set! and remove!
        (define path->mime (make-path->mime-type (collection-file-path "mime.types" "web-server" "default-web-root")))

        (define ~user {λ [~username] (with-handlers ([exn:fail:filesystem? {λ _ (find-system-path 'temp-dir)}]) (expand-user-path ~username))})
        (define ~path {λ [base pas] (cond [(false? (directory-exists? base)) (values base null)]
                                          [else (with-handlers ([exn? {λ _ (raise-user-error 'url->path "Found Escaping `..`!")}])
                                                  (let travel ([pieces null] [prst (map path/param-path pas)])
                                                    (match prst
                                                      [{cons {or 'up ".."} rst} (travel (cdr pieces) rst)]
                                                      [{cons {or 'same "." ""} rst} (travel pieces rst)]
                                                      [{cons pdir rst} (travel (cons pdir pieces) rst)]
                                                      [_ (let ([ps (reverse pieces)])
                                                           (values (simplify-path (apply build-path base ps) #false) ps))])))])})
        
        (define path->servlet {λ [->path mods] (let ([fns (make-make-servlet-namespace #:to-be-copied-module-specs mods)]
                                                     [tds (sakuyamon-timeout-default-servlet)])
                                                 (servlet:make-cached-url->servlet (filter-url->path #rx"\\.rkt$" (make-url->valid-path ->path))
                                                                                   (make-default-path->servlet #:timeouts-default-servlet tds
                                                                                                               #:make-servlet-namespace fns)))})
            
        
        (define dispatch
          {lambda [conn req]
            (with-handlers ([void {λ [?] ((lift:make {λ [req] (response:exn #false ? #"Dispatching")}) conn req)}])
              (define ->host {λ [host] (string->symbol (string-downcase (if (bytes? host) (bytes->string/utf-8 host) host)))})
              (define host (cond [(url-host (request-uri req)) => ->host]
                                 [(headers-assq* #"Host" (request-headers/raw req)) => (compose1 ->host header-value)]
                                 [else 'none]))
              (define paths (url-path (request-uri req)))
              (define-values {user? digimon?}
                (values (and (sakuyamon-user-terminus?) (false? (null? paths))
                             (let ([p (path/param-path (car paths))]) (and (string? p) (regexp-match? #"^[~].+$" p) p)))
                        (and (sakuyamon-digimon-terminus?) (false? (null? (cdr paths)))
                             (let ([p (path/param-path (cadr paths))]) (and (string? p) (regexp-match? #"^\\..+$" p) p)))))
              (define ::1? (string=? (request-client-ip req) "::1")) ; Note `::1` is a kind of local ip that happen to affect `host`
              ((hash-ref! dispatch-cache (list host user? digimon?)
                          {λ _ (parameterize ([current-custodian (current-server-custodian)])
                                 (chain:make (timeout:make initial-connection-timeout)
                                             (log:make #:format (log:log-format->format 'apache-default)
                                                       #:log-path (build-path (digimon-stone) "request.log"))
                                             (cond [(and user? digimon?) (dispatch-digimon user? digimon? ::1?)]
                                                   [(and user?) (dispatch-user user? ::1?)]
                                                   [else (dispatch-main ::1?)])
                                             (lift:make {λ [req] (response:404)})))})
               conn req))})
        
        (define dispatch-digimon
          {lambda [user digimon ::1?]
            (define ./htdocs (build-path "DigitalWorld" (string-trim digimon #px"\\." #:right? #false)
                                         (find-relative-path (digimon-zone) (digimon-tamer))
                                         (car (use-compiled-file-paths)) "handbook"))
            (chain:make (cond [::1? (chain:make)]
                              [else (pwd:make {λ [req] (let ([realm.rktl (build-path (~user user) ./htdocs 'up 'up "realm.rktl")])
                                                         (define-values {_ authorize} (pwd:password-file->authorized? realm.rktl))
                                                         ((pwd:make-basic-denied?/path authorize) req))}
                                              #:authentication-responder (λ [url header] (response:401 url header)))])
                        (filter:make #px"\\.rkt$" (lift:make {λ [req] (let-values ([{tail _} (~path "/" (drop (url-path (request-uri req)) 2))])
                                                                        (define dirtail (regexp-replaces (path->string tail)
                                                                                                         '{[#px"^/" ""]
                                                                                                           [#px"/" "_"]
                                                                                                           [#px"\\.rkt$" "_rkt/"]}))
                                                                        (define depth? (directory-exists? (build-path (~user user) ./htdocs dirtail)))
                                                                        (redirect-to (format "/~a/~a/~a" user digimon 
                                                                                             (cond [depth? dirtail]
                                                                                                   [else (string-replace dirtail #px"/$" ".html")]))))}))
                        (file:make #:path->mime-type path->mime
                                   #:url->path {λ [uri] (~path (build-path (~user user) ./htdocs) (drop (url-path uri) 2))}))})

        (define dispatch-user
          {lambda [user ::1?]
            (define /d-arc/ (string-append "/" user "/d-arc/"))
            (define ./htdocs (build-path "Public" "DigitalWorld" "terminus"))
            (define url->path {λ [uri] (~path (build-path (~user user) ./htdocs) (drop (url-path uri) 1))})
            (define-values {refresh! url->servlet} (path->servlet url->path null))
            (chain:make (filter:make (pregexp /d-arc/) (cond [(false? ::1?) (lift:make {λ _ (response:403)})]
                                                             [else (path:make (string-append /d-arc/ "refresh-servlet") {λ _ (response:rs refresh!)})]))
                        (timeout:make (sakuyamon-timeout-servlet-connection))
                        (servlet:make #:responders-servlet-loading (curryr response:exn #"Loading")
                                      #:responders-servlet (curryr response:exn #"Handling")
                                      url->servlet)
                        (file:make #:url->path url->path #:path->mime-type path->mime
                                   #:indices (list "default.html" "default.rkt")))})
        
        (define dispatch-main
          {lambda [::1?]
            (define htdocs (digimon-terminus))
            (define url->path {λ [uri] (~path htdocs (url-path uri))})
            (define-values {refresh! url->servlet} (path->servlet url->path null))
            (chain:make (filter:make #px"^/d-arc/" (cond [(false? ::1?) (lift:make {λ _ (response:403)})]
                                                         [else (chain:make (path:make "/d-arc/collect-garbage" {λ _ (response:gc)})
                                                                           (path:make "/d-arc/refresh-servlet" {λ _ (response:rs refresh!)}))]))
                        (timeout:make (sakuyamon-timeout-servlet-connection))
                        (servlet:make #:responders-servlet-loading (curryr response:exn #"Loading")
                                      #:responders-servlet (curryr response:exn #"Handling")
                                      url->servlet)
                        (file:make #:url->path url->path #:path->mime-type path->mime
                                   #:indices (list "default.html" "default.rkt")))})})
