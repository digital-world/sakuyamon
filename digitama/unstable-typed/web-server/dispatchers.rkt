#lang typed/racket

(provide (all-defined-out)
         (all-from-out (submod "." digitama))
         sequencer:make
         timeout:make
         filter:make
         lift:make
         pathprocedure:make
         log:make
         password:make
         host:make
         file:make
         servlet:make
         stat:make
         limit:make)

(define-type Dispatcher-Interface-Version (U 'v1))
(define-type Log-Format (U 'parenthesized-default 'extended 'apache-default))
(define-type Read-Request (-> Connection Integer (-> Input-Port (Values String String)) (Option Any)))

(define-type HTTP-Password (List* String #|domain|#
                                  String #|url regex|#
                                  (Listof (List Symbol #|name|#
                                                String #|password|#))))

(module digitama typed/racket
  ;;; This is a private submodule that provides shared definitions
  (provide (all-defined-out)
           Connection Request Response Can-Be-Response
           URL Path-Piece Header Binding
           Manager Stuffer Serializable)

  (require typed/net/url)
  (require typed/web-server/http)
  
  (require "private.rkt")
  (require "servlet.rkt")
  (require "managers.rkt")
  (require "stuffers.rkt")

  (define-type Servlet servlet)
  
  (define-type Dispatcher (-> Connection Request Void))
  (define-type URL->Path (-> URL (Values Path (Listof Path-Piece))))
  (define-type URL->Servlet (-> URL Servlet))
  (define-type Path->Servlet (-> Path Servlet))

  (define-type Over-Limit (U 'block 'kill-old 'kill-new))
  (define-type Format-Req (-> Request String))

  (define-type Denied? (-> Request (Option String)))
  (define-type Authorized? (-> String (Option Bytes) (Option Bytes) (Option String)))

  (require/typed/provide web-server/private/servlet
                         [#:struct servlet
                                   ([custodian : Custodian]
                                    [namespace : Namespace]
                                    [manager : Manager]
                                    [directory : Path-String]
                                    [handler : (-> Request Can-Be-Response)])
                                   #:extra-constructor-name make-servlet]))
(require (submod "." digitama))

(define-type Make-Servlet-Namespace
  (->* ()
       (#:additional-specs (Listof Module-Path))
       Namespace))

(require/typed/provide web-server/http/request
                       [read-request Read-Request]
                       [read-headers (-> Input-Port (Listof Header))]
                       [parse-bindings (-> Bytes (Listof Binding))]
                       [make-read-request (-> [#:connection-close? Boolean] Read-Request)])

(require/typed/provide web-server/dispatchers/dispatch
                       [#:struct exn:dispatcher () #:extra-constructor-name make-exn:dispatcher]
                       [next-dispatcher (-> Void)])

(require/typed/provide web-server/dispatchers/filesystem-map
                       [make-url->path (-> Path-String URL->Path)]
                       [make-url->valid-path (-> URL->Path URL->Path)]
                       [filter-url->path (-> Regexp URL->Path URL->Path)])

(require/typed/provide web-server/dispatchers/dispatch-log
                       [paren-format Format-Req]
                       [extended-format Format-Req]
                       [apache-default-format Format-Req]
                       [log-format->format (-> Log-Format Format-Req)])

(require/typed/provide web-server/dispatchers/dispatch-passwords
                       [make-basic-denied?/path (-> Authorized? Denied?)]
                       [password-file->authorized? (-> Path-String (Values (-> Void) Authorized?))])

(require/typed/provide web-server/dispatchers/dispatch-servlets
                       [make-cached-url->servlet (-> URL->Path Path->Servlet (Values (-> Void) URL->Servlet))])

(require/typed/provide web-server/configuration/namespace
                       [make-make-servlet-namespace
                        (-> #:to-be-copied-module-specs (Listof Module-Path)
                            Make-Servlet-Namespace)])

(require/typed/provide web-server/servlet/setup
                       [make-v1.servlet (-> Path-String Integer (Request -> Can-Be-Response) Servlet)]
                       [make-v2.servlet (-> Path-String Manager (Request -> Can-Be-Response) Servlet)]
                       [make-stateless.servlet
                        (-> Path-String
                            (Stuffer Serializable Bytes)
                            Manager
                            (Request -> Can-Be-Response)
                            Servlet)]
                       [make-default-path->servlet
                        (-> [#:make-servlet-namespace Make-Servlet-Namespace]	 
                            [#:timeouts-default-servlet Integer]
                            Path->Servlet)])

(module sequencer typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-sequencer
                 [make (-> Dispatcher * Dispatcher)]))

(module timeout typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-timeout
                 [make (-> Number Dispatcher)]))

(module lift typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-lift
                 [make (-> (-> Request Response) Dispatcher)]))

(module filter typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-filter
                 [make (-> Regexp Dispatcher Dispatcher)]))

(module pathprocedure typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-pathprocedure
                 [make (-> String (-> Request Response) Dispatcher)]))

(module log typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-log
                 [make (-> [#:format Format-Req]
                           [#:log-path Path-String]
                           Dispatcher)]))

(module passwords typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-passwords
                 [make (-> Denied?
                           [#:authentication-responder (-> URL Header Response)]
                           Dispatcher)]))

(module host typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-host
                 [make (-> (-> Symbol Dispatcher) Dispatcher)]))

(module files typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-files
                 [make (-> #:url->path URL->Path
                           [#:path->mime-type (-> Path (Option Bytes))]
                           [#:indices (Listof String)]
                           Dispatcher)]))

(module servlets typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-servlets
                 [make (-> URL->Servlet
                           [#:responders-servlet-loading (-> URL exn Can-Be-Response)]
                           [#:responders-servlet (-> URL exn Can-Be-Response)]
                           Dispatcher)]))

(module stat typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/dispatch-stat
                 [make (-> Dispatcher)]))

(module limit typed/racket
  (provide make)
  (require (submod ".." digitama))
  (require/typed web-server/dispatchers/limit
                 [make (-> Integer Dispatcher [#:over-limit Over-Limit] Dispatcher)]))

(require (prefix-in sequencer: (submod "." sequencer)))
(require (prefix-in timeout: (submod "." timeout)))
(require (prefix-in lift: (submod "." lift)))
(require (prefix-in filter: (submod "." filter)))
(require (prefix-in pathprocedure: (submod "." pathprocedure)))
(require (prefix-in log: (submod "." log)))
(require (prefix-in password: (submod "." passwords)))
(require (prefix-in host: (submod "." host)))
(require (prefix-in file: (submod "." files)))
(require (prefix-in servlet: (submod "." servlets)))
(require (prefix-in stat: (submod "." stat)))
(require (prefix-in limit: (submod "." limit)))
