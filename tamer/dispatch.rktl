#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Dispatching Rules!}

As an instance of the @bold{Racket} @deftech{Web Server},
@itech{Sakuyamon} is just a configuration of a dispatching server
that serves 3 types of @deftech[#:key "terminus"]{termini}, or @deftech{htdocs}.

Meanwhile, @hyperlink["http://en.wikipedia.org/wiki/Webserver_directory_index"]{directory indices}
are @litchar{default.rkt}, @litchar{index.html} and @litchar{index.htm}, respectively.
However they just exist on their own and not the directory@literal{'}s
@hyperlink["http://en.wikipedia.org/wiki/URL_redirection"]{redirection}s.

@tamer-smart-summary[]

@chunk[|<dispatch taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (define /htdocs (curry format "/~a"))
       (define /tamer (curry format "/~~~a/~a" (getenv "USER")))
       (define /digimon (curry format "/~~~a:~a/~a" (getenv "USER") (current-digimon)))

       (define ~htdocs (curry build-path (digimon-terminus)))
       (define ~tamer (curry build-path (find-system-path 'home-dir) "Public/DigitalWorld" "terminus"))
       (define ~digimon (curry build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook"))
       
       (define-values {shutdown curl 127.curl} (sakuyamon-realize))
       
       |<dispatch:*>|]

@para[#:style "GYDMWarning"]{For @hyperlink["http://en.wikipedia.org/wiki/Localhost"]{loopback addresses},
                                 @itech{Sakuyamon} always trust @deftech[#:key "trustable"]@litchar{::1} and treat
                                 @litchar{127.0.0.1} as a public one.}

@tamer-action[(curl "--help")
              (match-let ([{list _ _ headers _} (curl "-X" "Options" (/htdocs "/"))])
                (dict-ref headers 'allow))
              (match-let ([{list _ _ headers _} (curl "-X" "Options" (/tamer "/"))])
                (dict-ref headers 'allow))
              (match-let ([{list _ _ headers _} (curl "-X" "Options" (/digimon "/"))])
                (dict-ref headers 'allow))]

@handbook-scenario{Main Terminus}

@deftech{Main Terminus} is the major one shipped with @itech{Sakuyamon},
all URL paths other than ones of @itech{Per-Tamer Terminus} and @itech{Per-Digimon Terminus}
are relative to @racket[digimon-terminus].

@tamer-note['dispatch-main]

@chunk[|<testcase: dispatch main>|
       (test-spec "200: /robots.txt"
                  (match-let ([{list status reason _ _} (curl (/htdocs "robots.txt"))])
                    (check-eq? status 200 reason)))]

@deftech{Function URL}s are dispatched only when the requests come from @itech{trustable} clients.

@chunk[|<testcase: dispatch funtion URLs>|
       (for ([d-arc (in-list (list "d-arc/collect-garbage" "d-arc/refresh-servlet"))])
         (let ([rhtdocs /htdocs]) |<check: function url>|))]

@chunk[|<check: function url>|
       (test-case (format "200: ~a@::1" (rhtdocs d-arc))
                  (match-let ([{list status reason _ _} (curl (rhtdocs d-arc))])
                    (check-eq? status 200 reason)))
       (test-case (format "403: ~a@127" (rhtdocs d-arc))
                  (match-let ([{list status reason _ _} (127.curl (rhtdocs d-arc))])
                    (check-eq? status 403 reason)))]

@handbook-scenario{Per-Tamer Terminus}

@deftech{Per-Tamer Terminus} is designed for system users to share and discuss their works on the internet
if they store contents in the @deftech{public world} directory @litchar{$HOME/Public/DigitalWorld} and
organize them as a @hyperlink["https://github.com/digital-world/DigiGnome"]{digimon}. The first
@italic{path element} of URL always has the shape of @litchar{~user} and the rest parts
are relative to it@literal{'}s own @racket[digimon-terminus].

@tamer-note['dispatch-user]

@chunk[|<testcase: dispatch tamer>|
       (test-spec "200|404: /robots.txt"
                  (match-let ([{list status reason _ _} (curl (/tamer "robots.txt"))])
                    (check-pred (curryr member '{200 404}) status reason)))]

Note that @itech{Per-Tamer Terminus} do support @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")].
So users should be responsible for their own @itech{function URL}s.

@chunk[|<testcase: dispatch-user-funtion-URLs>|
       (for ([d-arc (in-list (list "d-arc/refresh-servlet"))])
         (let ([rhtdocs /tamer]) |<check: function url>|))]

Although users could write their own servlets to protect their contents in a more trustable way, they still have
HTTP @itech{DAA} to live a lazy life after putting the @itech{.realm.rktd} in their @itech{public world}.

@chunk[|<testsuite: digest access authentication>|
       (let*-values ([{type lhtdocs rhtdocs} (values 'digest ~tamer /tamer)]
                     [{.realm.rktd} (simple-form-path (lhtdocs 'up ".realm.rktd"))]
                     [{.realm-path} (path->string (file-name-from-path .realm.rktd))])
         (test-suite "Digest Authentication"
                     |<authenticate: setup and teardown>|
                     |<testcase: authentication>|))]

@chunk[|<testcase: authentication>|
       (let ([rpath (rhtdocs .realm-path)])
         (test-case (format "200: guest@::1:~a" type)
                    (match-let ([{list status reason _ _} (curl "--location" rpath)])
                      (check-eq? status 200 reason)))
         (test-case (format "401: guest@127:~a" type)
                    (match-let ([{list status reason _ _} (127.curl "--location" rpath)])
                      (check-eq? status 401 reason)))
         (test-case (format "200: wargrey@127:~a" type)
                    (match-let ([{list status reason _ _} (127.curl "--location" "--anyauth"
                                                                    "-u" "wargrey:gyoudmon" rpath)])
                      (check-eq? status 200 reason))))]

@para[#:style "GYDMError"]{After all the dynamic contents might be totally harmful for
                           @itech{Sakuyamon} and other @itech{tamers}, and there is no comprehensive
                           solution to defeat this problem. Therefore enabling @itech{Per-Tamer Terminus}
                           at your own risk!}

@handbook-scenario{Per-Digimon Terminus}

@deftech{Per-Digimon Terminus} is designed for system users to publish their project wikis like
@hyperlink["https://help.github.com/articles/what-are-github-pages/"]{Github Pages}. Projects
should be stored in directory @litchar{$HOME/DigitalWorld} and follow
@hyperlink["https://github.com/digital-world/DigiGnome"]{my project convientions}.
The first @italic{path element} of URL always has the shape of @litchar{~user:digimon}
and the rest parts are relative to @litchar{compiled/handbook} within @racket[digimon-tamer]
where stores the auto-generated @itech{htdocs}.

@para[#:style "GYDMWarning"]{Note its @litchar{robots.txt} should be placed in @racket[digimon-tamer]
                                      since it isn@literal{'}t the autogenerated one.}

@para[#:style "GYDMComment"]{@litchar{:} is a common separator works in lots of @italic{rc file}s
                              such as @litchar{/etc/passwd} in @italic{@bold{Unix-like} OS}es.}

@tamer-note['dispatch-digimon]

@chunk[|<testcase: dispatch digimon>|
       (test-spec "200: /robots.txt"
                  (let ([~digimon (curry build-path (digimon-tamer))])
                    (match-let ([{list status reason _ _} (curl (/digimon "robots.txt"))])
                    (check-eq? status 200 reason))))]

@itech{Per-Digimon Terminus} only serves static contents that usually be generated from the
@secref["scribble_lp2_Language" #:doc '(lib "scribblings/scribble/scribble.scrbl")].
So paths reference to any @litchar{*.rktl} will be redirected to their @litchar{*.html} counterparts.

@para[#:style "GYDMComment"]{Note @litchar{.rktl} stands for
                                  @secref["lp" #:doc '(lib "scribblings/scribble/scribble.scrbl")] 
                                  rather than its original intention:
                                  @secref["load-lang" #:doc '(lib "scribblings/reference/reference.scrbl")].}

Nonetheless, these paths would always be navigated by auto-generated navigators. All we need do is
making sure it works properly.

@chunk[|<testcase: URL Rewriting>|
       (for ([rpath (in-list (list "!/../." "./t/h.lp.rktl" "../../tamer.rkt"))]
             [px (in-list (list #px"/$" #px"t_h_lp_rktl(/|\\.html)$" #false))]
             [expect (in-list (list 302 302 418))])
         (make-directory* (~digimon))
         (test-case (format "~a: ~a" expect rpath)
                    (match-let ([{list status reason headers _} (curl (/digimon rpath))])
                      (check-eq? status expect reason)
                      (when (and (= expect 302) (regexp? px))
                        (let ([location (dict-ref headers 'location)])
                          (check-regexp-match px location))))))]

Sometimes, users may not want to publish their projects@literal{'} documentation, although this is not recommended.
Nonetheless, @itech{Per-Digimon Terminus} do support HTTP @itech{BAA}, and a @itech{.realm.rktd} within @racket[digimon-tamer]
is required to work with @secref["dispatch-passwords" #:doc '(lib "web-server/scribblings/web-server-internal.scrbl")].

@chunk[|<testsuite: basic access authentication>|
       (let*-values ([{type lhtdocs rhtdocs} (values 'basic ~digimon /digimon)]
                     [{.realm.rktd} (simple-form-path (lhtdocs 'up 'up ".realm.rktd"))]
                     [{.realm-path} (path->string (file-name-from-path .realm.rktd))])
         (test-suite "Basic Authentication"
                     |<authenticate: setup and teardown>|
                     |<testcase: authentication>|))]

By the way, as you may guess, users don@literal{'}t need to refresh passwords manually
since the @itech{.realm.rktd} is checked every request.

@para[#:style "GYDMWarning"]{@itech{Per-Digimon Terminus} is disabled by default.}

@handbook-appendix[]

@chunk[|<dispatch:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define sakuyamon (parameterize ([current-command-line-arguments (vector)]
                                          [current-output-port /dev/null]
                                          [current-error-port /dev/null]
                                          [exit-handler void])
                             (dynamic-require (build-path (digimon-digivice) "sakuyamon.rkt") 'main)))
         
         (define-tamer-suite dispatch-main "Main Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch main>|
           (test-suite "Function URLs" |<testcase: dispatch funtion URLs>|))

         (define-tamer-suite dispatch-user "Per-Tamer Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch tamer>|
           (list (test-suite "Function URLs" |<testcase: dispatch-user-funtion-URLs>|)
                 |<testsuite: digest access authentication>|))

         (define-tamer-suite dispatch-digimon "Per-Digimon Terminus"
           |<dispatch: check #:before>| #:after {λ _ (shutdown)}
           |<testcase: dispatch digimon>|
           (list (test-suite "URL Rewriting" |<testcase: URL Rewriting>|)
                 |<testsuite: basic access authentication>|))}]

@chunk[|<dispatch: check #:before>|
       #:before {λ _ (when (string? 127.curl) (raise-result-error 'realize "procedure?" 127.curl))}]

@chunk[|<authenticate: setup and teardown>|
       #:before {λ _ (let ([.realm.dtkr (path-replace-suffix .realm.rktd ".dtkr")])
                       (for-each make-directory* (list (path-only .realm.rktd) (lhtdocs)))
                       (when (file-exists? .realm.rktd)
                         (rename-file-or-directory .realm.rktd .realm.dtkr))
                       (if (symbol=? type 'digest)
                           (with-output-to-file .realm.rktd #:exists 'replace
                             {λ _ (parameterize ([exit-handler void])
                                    (sakuyamon "realm" realm.rktd))})
                           (copy-file realm.rktd .realm.rktd #true))
                       (copy-file .realm.rktd (lhtdocs .realm-path)))}
       #:after {λ _ (let ([.realm.dtkr (path-replace-suffix .realm.rktd ".dtkr")])
                      (for-each delete-file (list .realm.rktd (lhtdocs .realm-path)))
                      (when (file-exists? .realm.dtkr)
                        (rename-file-or-directory .realm.dtkr .realm.rktd)))}]
