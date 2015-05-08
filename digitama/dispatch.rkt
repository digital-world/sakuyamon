#lang at-exp racket

@require{digicore.rkt}
@require{http.rkt}

(require net/tcp-unit)
(require net/ssl-tcp-unit)

(require (only-in web-server/http make-header))
(require web-server/servlet/setup)
(require web-server/dispatchers/dispatch)
(require web-server/dispatchers/filesystem-map)

(require web-server/private/mime-types)
(require web-server/private/dispatch-server-sig)
(require web-server/private/web-server-structs)

(require (prefix-in http: web-server/http/request))
(require (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer))
(require (prefix-in timeout: web-server/dispatchers/dispatch-timeout))
(require (prefix-in passwords: web-server/dispatchers/dispatch-passwords))
(require (prefix-in files: web-server/dispatchers/dispatch-files))
(require (prefix-in servlets: web-server/dispatchers/dispatch-servlets))
(require (prefix-in path: web-server/dispatchers/dispatch-pathprocedure))
(require (prefix-in log: web-server/dispatchers/dispatch-log))
(require (prefix-in filter: web-server/dispatchers/dispatch-filter))
(require (prefix-in lift: web-server/dispatchers/dispatch-lift))

(provide sakuyamon-tcp@ sakuyamon-config@)

(define sakuyamon-tcp@ (lazy (cond [(false? (sakuyamon-ssl?)) tcp@]
                                   [else (let ([sakuyamon.crt (build-path (sakuyamon-config) (format "~a.crt" (current-digimon)))]
                                               [sakuyamon.key (build-path (sakuyamon-config) (format "~a.key" (current-digimon)))])
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
        (define stone/ (curry build-path (sakuyamon-config)))
        (define dispatch
          {lambda [conn req]
            (with-handlers ([exn? {λ [e] ((dispatch-exception (request-uri req) e) conn req)}])
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
              ((hash-ref! dispatch-cache (list host user? digimon?)
                          {λ _ (parameterize ([current-custodian (current-server-custodian)])
                                 (define theader (make-header #"Terminus" (cond [(and user? digimon?) #"Per-Digimon"]
                                                                                [user? #"Per-User"]
                                                                                [else #"Main"])))
                                 (sequencer:make (timeout:make initial-connection-timeout)
                                                 (log:make #:format (log:log-format->format 'extended)
                                                           #:log-path (build-path (sakuyamon-config) "access.log"))
                                                 (cond [(and user? digimon?) (dispatch-digimon user? digimon?)]
                                                       [(and user?) (dispatch-user user?)]
                                                       [(false? (string=? "::1" (request-client-ip req))) (dispatch-main)]
                                                       [else (dispatch-manager dispatch-main)])
                                                 (path:make "/error.css" {λ _ (file-response 200 #"Okay" (stone/ "error.css") theader)})
                                                 (lift:make {λ [req] (response:404 (stone/ "file-not-found.html") theader)})))})
               conn req))})
        
        (define dispatch-digimon
          {lambda [user digimon]
            (files:make #:path->mime-type (make-path->mime-type (stone/ "mime.types"))
                        #:url->path {λ [URL] (with-handlers ([exn? {λ [e] (next-dispatcher)}])
                                               (define htdocs (build-path (expand-user-path user) "DigitalWorld"
                                                                          (string-trim digimon #px"\\." #:right? #false)
                                                                          (find-relative-path (digimon-zone) (digimon-tamer))
                                                                          (car (use-compiled-file-paths))
                                                                          "handbook"))
                                               (define felems (filter-map {λ [pa] (match (path/param-path pa)
                                                                                    ['up ".."]
                                                                                    [{or 'same ""} #false]
                                                                                    [p (string-replace p #px"\\.rkt$" "_rkt.html")])}
                                                                          (drop (url-path URL) 2)))
                                               (values (apply build-path htdocs felems) felems))})})

        (define dispatch-user
          {lambda [user]
            #|(let-values ([{clear-cache! url->servlet} (servlets:make-cached-url->servlet
                                                      (filter-url->path #rx"\\.(ss|scm|rkt|rktd)$"
                                                                        (make-url->valid-path (make-url->path (paths-servlet (host-paths host-info)))))
                                                      (make-default-path->servlet #:make-servlet-namespace config:make-servlet-namespace
                                                                                  #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info))))])
             (sequencer:make (path-procedure:make "/conf/refresh-servlets" {λ _
                                                                             (clear-cache!)
                                                                             ((responders-servlets-refreshed (host-responders host-info)))})
                             (sequencer:make (timeout:make (timeouts-servlet-connection (host-timeouts host-info)))
                                             (servlets:make url->servlet
                                                            #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                                                            #:responders-servlet (responders-servlet (host-responders host-info))))))|#
                          ;(files:make #:url->path (make-url->path (paths-htdocs (host-paths host-info)))
                           ;           #:path->mime-type (make-path->mime-type (paths-mime-types (host-paths host-info)))
                            ;          #:indices (host-indices host-info))
            (sequencer:make)})
        
        (define dispatch-main
          {lambda []
          #|(let-values ([{clear-cache! url->servlet} (servlets:make-cached-url->servlet
                                                      (filter-url->path #rx"\\.(ss|scm|rkt|rktd)$"
                                                                        (make-url->valid-path (make-url->path (paths-servlet (host-paths host-info)))))
                                                      (make-default-path->servlet #:make-servlet-namespace config:make-servlet-namespace
                                                                                  #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info))))])
             (sequencer:make (path-procedure:make "/conf/refresh-servlets" {λ _
                                                                             (clear-cache!)
                                                                             ((responders-servlets-refreshed (host-responders host-info)))})
                             (sequencer:make (timeout:make (timeouts-servlet-connection (host-timeouts host-info)))
                                             (servlets:make url->servlet
                                                            #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                                                            #:responders-servlet (responders-servlet (host-responders host-info))))))|#
                          ;(files:make #:url->path (make-url->path (paths-htdocs (host-paths host-info)))
                           ;           #:path->mime-type (make-path->mime-type (paths-mime-types (host-paths host-info)))
                            ;          #:indices (host-indices host-info))
            (sequencer:make)})

        (define dispatch-manager
          {lambda [fnext-dispatcher]
            (sequencer:make (path:make "/conf/collect-garbage" {λ _ (response:gc)})
                            #|(let-values ([{clear-cache! url->servlet} (servlets:make-cached-url->servlet
                                                                       (filter-url->path #rx"\\.(ss|scm|rkt|rktd)$"
                                                                                         (make-url->valid-path (make-url->path (paths-servlet (host-paths host-info)))))
                                                                       (make-default-path->servlet #:make-servlet-namespace config:make-servlet-namespace
                                                                                                   #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info))))])
                              (sequencer:make (path-procedure:make "/conf/refresh-servlets" {λ _
                                                                                              (clear-cache!)
                                                                                              ((responders-servlets-refreshed (host-responders host-info)))})))
                            |#
                            (fnext-dispatcher))})
        
        (define dispatch-exception
          {lambda [url exception]
            (lift:make {λ [req] (parameterize ([error-display-handler void])
                                  (response:exn url exception))})})})