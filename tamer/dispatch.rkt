#lang scribble/lp2

@(require "tamer.rkt")

@handbook-story{Dispatching and HTTP Access Authentication!}

As an instance of @bold{Racket} @deftech{Web Server},
@itech{Sakuyamon} is just a configuration of a dispatching server
that serves 3 types of @deftech[#:key "terminus"]{termini}, or @deftech{htdocs}.

@tamer-smart-summary[]

@chunk[|<dispatch taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       (module+ tamer |<dispatch:*>|)]

@racketcommentfont{For @hyperlink["http://en.wikipedia.org/wiki/Localhost"]{@racketcommentfont{loopback addresses}},
                    @itech{@racketcommentfont{Sakuyamon}} always trust @deftech[#:key "trusted"]@litchar{::1} and treat
                    @litchar{127.0.0.1} as a public one.}

@tamer-action[(with-handlers ([exn:fail:user? (compose1 displayln exn-message)])
                (curl "--help"))]

@handbook-scenario{Main Terminus}

@deftech{Main Terminus} is the major one shipped with @itech{Sakuyamon},
all URL paths other than ones of @itech{Per-Tamer Terminus} and @itech{Per-Digimon Terminus}
are relative to @racket[digimon-terminus].

@tamer-note['dispatch-main]

@chunk[|<testcase: dispatch main>|
       (test-case "HTTP OPTIONS"
                  (match-let ([(list _ _ headers _) (curl "-X" "Options" (/htdocs "robots.txt"))])
                    (check-regexp-match #px"^((GET|HEAD|POST),?){3}$" (dict-ref headers 'allow))
                    (check-equal? (dict-ref headers 'terminus) "Main")))]

@deftech{Function URL}s are dispatched only when the requests come from @itech{trusted} clients.

@chunk[|<testcase: dispatch funtion URLs>|
       (for ([d-arc (in-list (list "d-arc/collect-garbage" "d-arc/refresh-servlet"))])
         (let ([rhtdocs /htdocs]) |<check: function url>|))]

@chunk[|<check: function url>|
       (test-case (format "200: ~a@::1" (rhtdocs d-arc))
                  (match-let ([(list status reason _ _) (curl (rhtdocs d-arc))])
                    (check-eq? status 200 reason)))
       (test-case (format "403: ~a@127" (rhtdocs d-arc))
                  (match-let ([(list status reason _ _) (127.curl (rhtdocs d-arc))])
                    (check-eq? status 403 reason)))]

@handbook-scenario{Per-Tamer Terminus}

@deftech{Per-Tamer Terminus} is designed for system users to share and discuss their work on the internet
if they organize it as a @hyperlink[(format "https://github.com/digital-world/~a#digital-world" (digimon-gnome))]{digimon}
called @deftech{@(digimon-kuzuhamon)}. The first @italic{path element} of URL always has the shape of @litchar{~username}
and the rest parts are relative to its own @racket[digimon-terminus]s.

@tamer-note['dispatch-tamer]

@chunk[|<testcase: dispatch tamer>|
       (test-case "HTTP OPTIONS"
                  (match-let ([(list _ _ headers _) (curl "-X" "Options" (/tamer "robots.txt"))])
                    (check-regexp-match #px"^((GET|HEAD|POST),?){3}$" (dict-ref headers 'allow))
                    (check-equal? (dict-ref headers 'terminus) "Per-Tamer")))]

Note that @itech{Per-Tamer Terminus} does support @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")].
So users should be responsible for their own @itech{function URL}s.

@chunk[|<testcase: dispatch-tamer-funtion-URLs>|
       (for ([d-arc (in-list (list "d-arc/refresh-servlet"))])
         (let ([rhtdocs /tamer]) |<check: function url>|))]

Although users could write their own servlets to protect their contents in a more security way, they still have
HTTP @itech{DAA} to live a lazy life after putting the @itech{.realm.rktd} in the root of @itech{@(digimon-kuzuhamon)}.

@chunk[|<testsuite: digest access authentication>|
       (let*-values ([(type lhtdocs rhtdocs) (values 'digest ~tamer /tamer)]
                     [(.realm.rktd) (simple-form-path (lhtdocs 'up ".realm.rktd"))]
                     [(.realm-path) (path->string (file-name-from-path .realm.rktd))])
         (test-suite "Digest Authentication"
                     |<authenticate: setup and teardown>|
                     |<testcase: authentication>|))]

@chunk[|<testcase: authentication>|
       (let ([rpath (rhtdocs .realm-path)])
         (test-case (format "200|503: guest@::1:~a" type)
                    (match-let ([(list status reason _ _) (curl "--location" rpath)])
                      (check-eq? status (if root? 503 200) reason)))
         (test-case (format "401|503: guest@127:~a" type)
                    (match-let ([(list status reason _ _) (127.curl "--location" rpath)])
                      (check-eq? status (if root? 503 401) reason)))
         (test-case (format "200|503: wargrey@127:~a" type)
                    (match-let ([(list status reason _ _) (127.curl "--location" "--anyauth"
                                                                    "-u" "wargrey:gyoudmon" rpath)])
                      (check-eq? status (if root? 503 200) reason))))]

By the way, users can custom @hyperlink["http://en.wikipedia.org/wiki/HTTP_404"]{404 pages} by putting a
@litchar{404.html} in their own @racket[digimon-stone].

@racketresultfont{After all the dynamic contents might be totally harmful for other
                   @itech{@racketresultfont{tamer}}s if they are members of the @litchar{tamer} group.
                   Therefore enabling @itech{@racketresultfont{Per-Tamer Terminus}} at your own risk!}

@handbook-scenario{Per-Digimon Terminus}

@deftech{Per-Digimon Terminus} is designed for system users to publish their project wikis like
@hyperlink["https://help.github.com/articles/what-are-github-pages/"]{Github Pages}, and projects
should be organized in the @hyperlink[(format "https://github.com/digital-world/~a#digital-world" (digimon-gnome))]{Digital World}.
The first @italic{path element} of URL always has the shape of @litchar{~username:digimon}
and the rest parts are relative to @litchar{compiled/handbook} within their own @racket[digimon-tamer]
where stores the auto-generated @itech{htdocs}.

@racketcommentfont{Note its @litchar{robots.txt} should be placed in @racket[digimon-tamer]
                    since it isn@literal{'}t an auto-generated resource.}

@tamer-note['dispatch-digimon]

@chunk[|<testcase: dispatch digimon>|
       (test-case "HTTP OPTIONS"
                  (match-let ([(list _ _ headers _) (curl "-X" "Options" (/digimon "robots.txt"))])
                    (check-regexp-match #px"^((GET|HEAD),?){2}$" (dict-ref headers 'allow))
                    (check-equal? (dict-ref headers 'terminus) "Per-Digimon")))]

Sometimes, users may not want to publish their projects@literal{'} documentation, although this is not recommended.
Nonetheless, @itech{Per-Digimon Terminus} do support HTTP @itech{BAA}, and a @itech{.realm.rktd} within @racket[digimon-tamer]
is required to work with @secref["dispatch-passwords" #:doc '(lib "web-server/scribblings/web-server-internal.scrbl")].

@chunk[|<testsuite: basic access authentication>|
       (let*-values ([(type lhtdocs rhtdocs) (values 'basic ~digimon /digimon)]
                     [(.realm.rktd) (simple-form-path (lhtdocs 'up 'up ".realm.rktd"))]
                     [(.realm-path) (path->string (file-name-from-path .realm.rktd))])
         (test-suite "Basic Authentication"
                     |<authenticate: setup and teardown>|
                     |<testcase: authentication>|))]

By the way, as you may guess, users don@literal{'}t need to refresh passwords manually
since the @itech{.realm.rktd} is checked every request.

@racketcommentfont{@itech{@racketcommentfont{Per-Digimon Terminus}} is disabled by default.}

@handbook-bibliography[]

@chunk[|<dispatch:*>|
       (module+ story
         (define sakuyamon (parameterize ([current-command-line-arguments (vector)]
                                          [current-output-port /dev/null]
                                          [current-error-port /dev/null]
                                          [exit-handler void])
                             (dynamic-require (build-path (digimon-digivice) "sakuyamon.rkt") 'main)))
         
         (define-tamer-suite dispatch-main "Main Terminus"
           #:before (check-port-ready? tamer-sakuyamon-port #:type todo)
           |<testcase: dispatch main>|
           (test-suite "Function URLs" |<testcase: dispatch funtion URLs>|))

         (define-tamer-suite dispatch-tamer "Per-Tamer Terminus"
           #:before (check-port-ready? tamer-sakuyamon-port #:type todo)
           |<testcase: dispatch tamer>|
           (list (test-suite "Function URLs" |<testcase: dispatch-tamer-funtion-URLs>|)
                 |<testsuite: digest access authentication>|))

         (define-tamer-suite dispatch-digimon "Per-Digimon Terminus"
           #:before (check-port-ready? tamer-sakuyamon-port #:type todo)
           |<testcase: dispatch digimon>|
           |<testsuite: basic access authentication>|))]

@chunk[|<authenticate: setup and teardown>|
       #:before (thunk (unless root?
                         (let ([.realm.dtkr (path-replace-suffix .realm.rktd ".dtkr")])
                           (for-each make-directory* (list (path-only .realm.rktd) (lhtdocs)))
                           (when (file-exists? .realm.rktd)
                             (rename-file-or-directory .realm.rktd .realm.dtkr))
                           (if (symbol=? type 'digest)
                               (with-output-to-file .realm.rktd #:exists 'replace
                                 (thunk (parameterize ([exit-handler void])
                                          (sakuyamon "sphere" realm.rktd))))
                               (copy-file realm.rktd .realm.rktd #true))
                           (copy-file .realm.rktd (lhtdocs .realm-path)))))
       #:after (thunk (unless root?
                        (let ([.realm.dtkr (path-replace-suffix .realm.rktd ".dtkr")])
                          (for-each delete-file (list .realm.rktd (lhtdocs .realm-path)))
                          (when (file-exists? .realm.dtkr)
                            (rename-file-or-directory .realm.dtkr .realm.rktd)))))]
