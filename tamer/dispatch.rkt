#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Dispatching Rules!}

As an instance of the @bold{Racket} @deftech{Web Server},
@itech{Sakuyamon} is just a configuration of a dispatching server
that serves 3 types of @deftech[#:key "terminus"]{termini}, or @deftech{htdocs}.

By default, @itech{Per-User Terminus} and @itech{Per-Digimon Terminus} are disabled
since they are system-wide @itech[#:key "Terminus"]{Termini}.

@tamer-smart-summary[]

@chunk[|<dispatch taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       (define-values {shutdown sendrecv} (sakuyamon-realize))
       |<dispatch:*>|]

@handbook-scenario{Main Terminus}

@deftech{Main Terminus} is the major one shipped with @itech{Sakuyamon},
all URL paths other than ones of @itech{Per-User Terminus} and @itech{Per-Digimon Terminus}
are relative to @racket[digimon-terminus].

@tamer-note['dispatch-main]

@chunk[|<testcase: dispatch-main>|
       (for ([rpath (in-list (list ".digimon/~user/readme.t"))])
         (let ([lpath (build-path (digimon-terminus) rpath)]) 
           (test-spec rpath |<dispatch: setup and teardown>| |<dispatch: check>|)))]

For the sake of security, these @deftech{function URL}s are dispatched only when the client is @litchar{::1}.

@chunk[|<testcase: dispatch-funtion-URLs>|
       (for ([d-arc (in-list (list "/d-arc/collect-garbage" "/d-arc/refresh-servlet"))])
         (test-case (format "[::1]~a" d-arc)
                    (let-values ([{status brief headers /dev/net/stdin} (sendrecv d-arc)])
                      (check-eq? status 200 brief)))
         (test-case (format "[127.0.0.1]~a" d-arc)
                    (let-values ([{status brief headers /dev/net/stdin} (sendrecv d-arc #:host "127.0.0.1")])
                      (check-eq? status 403 brief))))]

@handbook-scenario{Per-User Terminus}

@deftech{Per-User Terminus} is designed for system users to share and discuss their works on the internet
if they store the content in the directory @litchar{$HOME/Public/DigitalWorld}.
URL paths always start with @litchar{~user}.

@tamer-note['dispatch-user]

@chunk[|<testcase: dispatch-user>|
       (for ([path (in-list (list "readme.t"))])
         (let ([rpath (format "/~~~a/~a" (getenv "USER") path)]
               [lpath (build-path (find-system-path 'home-dir) "Public" "DigitalWorld" path)]) 
           (test-spec path |<dispatch: setup and teardown>| |<dispatch: check>|)))]

@bold{Note that this @itech{Terminus} do support @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")].}

@handbook-scenario{Per-Digimon Terminus}

@deftech{@bold{Per-Digimon Terminus}} is designed for system users to publish their project wikis like
@hyperlink["https://help.github.com/articles/what-are-github-pages/"]{Github Pages}. Public projects
should be stored in directory @litchar{$HOME/DigitalWorld} and follow
@hyperlink["https://github.com/digital-world/DigiGnome"]{my project convientions}.
URL paths always start with @litchar{~user} followed by the @italic{path element} @litchar{.digimon},
and the rest parts are relative to @litchar{compiled/handbook} within @racket[digimon-tamer]
where stores the auto-generated @itech{htdocs}.

@tamer-note['dispatch-digimon]

@chunk[|<testcase: dispatch-digimon>|
       (for ([path (in-list (list "readme.t"))])
         (let ([rpath (format "/~~~a/.~a/~a" (getenv "USER") (current-digimon) path)]
               [lpath (build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook" path)]) 
           (test-spec path |<dispatch: setup and teardown>| |<dispatch: check>|)))]

This @itech{Terminus} is the simplest one since it only serves static content.
By default, users should ignore the name convention of @bold{Scribble},
so paths reference to any @litchar{*.rkt} will be transformed to their @litchar{*.html} counterparts
iff they are valid @secref["scribble_lp2_Language" #:doc '(lib "scribblings/scribble/scribble.scrbl")]s.

Nonetheless, these paths would always be navigated by auto-generated navigators. All we need do is
making sure it works properly.

@chunk[|<testcase: rewrite-url>|
       (for ([rpath (in-list (list "?/../" "../index.html" "../../tamer.rkt" "!/../dispatch.rkt"))]
             [expect (in-list (list 200 418 418 200))])
         (test-case (format "~a: ~a" expect rpath)
                    (let-values ([{status brief headers /dev/net/stdin} (sendrecv (~htdocs rpath))]
                                 [{lpath} (format "~a/compiled/handbook/~a" (digimon-tamer) rpath)])
                      (with-handlers ([exn:test:check? {λ [f] (cond [(file-exists? lpath) (raise f)]
                                                                    [else (check-eq? status 404 brief)])}])
                        (check-eq? status expect brief)))))]

@handbook-appendix[]

@chunk[|<dispatch:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite dispatch-main "Main Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch-main>|
           (test-suite "Function URLs" |<testcase: dispatch-funtion-URLs>|))

         (define-tamer-suite dispatch-user "Per-User Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch-user>|)

         (define-tamer-suite dispatch-digimon "Per-Digimon Terminus"
           |<dispatch: check #:before>| #:after {λ _ (shutdown)}
           |<testcase: dispatch-digimon>|
           (let ([~htdocs (curry format "/~~~a/.~a/~a" (getenv "USER") (current-digimon))])
             (test-suite "Rewrite URL" |<testcase: rewrite-url>|)))}]

@chunk[|<dispatch: check #:before>|
       #:before {λ _ (when (pair? sendrecv) (raise-result-error 'realize "procedure?" sendrecv))}]

@chunk[|<dispatch: setup and teardown>|
       #:before {λ _ (void (make-parent-directory* lpath)
                           (display-to-file lpath lpath))}
       #:after {λ _ (void (delete-file lpath)
                          (with-handlers ([exn? void])
                            (let rmdir ([dir (path-only lpath)])
                              (delete-directory dir)
                              (rmdir (build-path dir 'up)))))}]

@chunk[|<dispatch: check>|
       (let-values ([{status brief headers /dev/net/stdin} (sendrecv rpath)])
         (check-eq? status 200 brief)
         (check-equal? (read-line /dev/net/stdin) (path->string lpath)))]
