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
       (define {127-sendrecv uri #:method [method #"GET"] #:headers [headers null] #:data [data #false]}
         (sendrecv uri #:host "127.0.0.1" #:method method #:headers headers #:data data))
       |<dispatch:*>|]

@handbook-scenario{Main Terminus}

@deftech{Main Terminus} is the major one shipped with @itech{Sakuyamon},
all URL paths other than ones of @itech{Per-User Terminus} and @itech{Per-Digimon Terminus}
are relative to @racket[digimon-terminus].

@tamer-note['dispatch-main]

@chunk[|<testcase: dispatch main>|
       (for ([rpath (in-list (list ".digimon/~user/readme.t"))])
         (let ([lpath (build-path (digimon-terminus) rpath)]) 
           (test-spec rpath |<dispatch: setup and teardown>| |<check: dispatch>|)))]

@chunk[|<check: dispatch>|
       (match-let ([{list status reason _ /dev/net/stdin} (sendrecv rpath)])
         (check-eq? status 200 reason)
         (check-equal? (read-line /dev/net/stdin) (path->string lpath)))]

For the sake of security, these @deftech{function URL}s are dispatched only when the client is @litchar{::1}.

@chunk[|<testcase: dispatch funtion URLs>|
       (for ([d-arc (in-list (list "d-arc/collect-garbage" "d-arc/refresh-servlet"))])
         |<check: function url>|)]

@chunk[|<check: function url>|
       (test-case (format "[::1]/~a" d-arc)
                  (match-let ([{list status reason _ _} (sendrecv (~htdocs d-arc))])
                    (check-eq? status 200 reason)))
       (test-case (format "[127]/~a" d-arc)
                  (match-let ([{list status reason _ _} (127-sendrecv (~htdocs d-arc))])
                    (check-eq? status 403 reason)))]

@handbook-scenario{Per-User Terminus}

@deftech{Per-User Terminus} is designed for system users to share and discuss their works on the internet
if they store the content in the directory @litchar{$HOME/Public/DigitalWorld} and organise it as a
@hyperlink["https://github.com/digital-world/DigiGnome"]{digimon}. URL paths always start with @litchar{~user}
and the rest parts are relative to @litchar{terminus}.

@tamer-note['dispatch-user]

@chunk[|<testcase: dispatch user>|
       (for ([path (in-list (list "readme.t"))])
         (let ([rpath (format "/~~~a/~a" (getenv "USER") path)]
               [lpath (build-path (find-system-path 'home-dir) "Public/DigitalWorld" "terminus" path)]) 
           (test-spec path |<dispatch: setup and teardown>| |<check: dispatch>|)))]

Note that @itech{Per-User Terminus} do support @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")].
So users should be responsible for their own @itech{function URL}s through @litchar{::1}.

@chunk[|<testcase: dispatch-user-funtion-URLs>|
       (for ([d-arc (in-list (list "d-arc/refresh-servlet"))])
         |<check: function url>|)]

@handbook-scenario{Per-Digimon Terminus}

@deftech{Per-Digimon Terminus} is designed for system users to publish their project wikis like
@hyperlink["https://help.github.com/articles/what-are-github-pages/"]{Github Pages}. Projects
should be stored in directory @litchar{$HOME/DigitalWorld} and follow
@hyperlink["https://github.com/digital-world/DigiGnome"]{my project convientions}.
URL paths always start with @litchar{~user} followed by the @italic{path element} @litchar{.digimon},
and the rest parts are relative to @litchar{compiled/handbook} within @racket[digimon-tamer]
where stores the auto-generated @itech{htdocs}.

@tamer-note['dispatch-digimon]

@chunk[|<testcase: dispatch digimon>|
       (for ([path (in-list (list "readme.t"))])
         (let ([rpath (format "/~~~a/.~a/~a" (getenv "USER") (current-digimon) path)]
               [lpath (build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook" path)]) 
           (test-spec path |<dispatch: setup and teardown>| |<check: dispatch>|)))]

@itech{Per-Digimon Terminus} is the simplest one since it only serves static content.
By default, users should ignore the name convention of @bold{Scribble},
so paths reference to any @litchar{*.rkt} will be transformed to their @litchar{*.html} counterparts
iff they are valid @secref["scribble_lp2_Language" #:doc '(lib "scribblings/scribble/scribble.scrbl")]s.

Nonetheless, these paths would always be navigated by auto-generated navigators. All we need do is
making sure it works properly.

@chunk[|<testcase: rewrite url>|
       (for ([rpath (in-list (list "!/../."  "!/../dispatch.rkt" "dir/lp.rkt" "./../../tamer.rkt"))]
             [px (in-list (list #px"/$" #px"_rkt(/|\\.html)$" #px"dir_lp_rkt(/|\\.html)$" #false))]
             [expect (in-list (list 302 302 302 418))])
         (test-case (format "~a: ~a" expect rpath)
                    (match-let ([{list status reason headers _} (sendrecv (~htdocs rpath))])
                      (check-eq? status expect reason)
                      (when (and (= expect 302) (regexp? px))
                        (check-regexp-match px (bytes->string/utf-8 (dict-ref headers #"Location")))))))]

Sometimes, users may want to hide their private projects, although this is not recommended.
Nonetheless, @itech{Per-Digimon Terminus} do support
@hyperlink["http://en.wikipedia.org/wiki/Basic_access_authentication"]{HTTP Basic Access Authentication}.
Just put a file @litchar{realm.rktl} in the @racket[digimon-tamer] to work with
@secref["dispatch-passwords" #:doc '(lib "web-server/scribblings/web-server-internal.scrbl")].

@chunk[|<testcase: basic access authentication>|
       (let* ([realm.rktl (build-path (digimon-tamer) "realm.rktl")]
              [client {λ [sendrecv #:username [user #false] #:password [pwd #false]]
                        (sakuyamon-agent sendrecv (~htdocs ".") #"GET" #:username user #:password pwd)}])
         (test-suite "Basic Authentication"
                     #:before {λ _ (with-output-to-file realm.rktl #:exists 'error
                                     {λ _ (printf "'~s~n" '{{"realm" "(#px)?/.+"
                                                                     [user "password"]
                                                                     [tamer "opensource"]}})})}
                     #:after {λ _ (delete-file realm.rktl)}
                     (test-case "200: [::1]guest"
                                (match-let ([{list status reason _ _} (client sendrecv)])
                                  (check-eq? status 200 reason)))
                     (test-case "401: [127]guest"
                                (match-let ([{list status reason _ _} (client 127-sendrecv)])
                                  (check-eq? status 401 reason)))
                     (test-case "200: [127]tamer"
                                (match-let ([{list status reason _ _} (client 127-sendrecv
                                                                              #:username #"tamer"
                                                                              #:password #"opensource")])
                                  (check-eq? status 200 reason)))))]

By the way, as you may guess, users don@literal{'}t need to refresh passwords manually
since the @litchar{realm.rktl} is checked every request. After all
the authentication is transparent to the client @litchar{::1}.

@handbook-appendix[]

@chunk[|<dispatch:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite dispatch-main "Main Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch main>|
           (let ([~htdocs (curry format "/~a")])
            (test-suite "Function URLs" |<testcase: dispatch funtion URLs>|)))

         (define-tamer-suite dispatch-user "Per-User Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch user>|
           (let ([~htdocs (curry format "/~~~a/~a" (getenv "USER"))])
             (test-suite "Function URLs" |<testcase: dispatch-user-funtion-URLs>|)))

         (define-tamer-suite dispatch-digimon "Per-Digimon Terminus"
           |<dispatch: check #:before>| #:after {λ _ (shutdown)}
           |<testcase: dispatch digimon>|
           (let ([~htdocs (curry format "/~~~a/.~a/~a" (getenv "USER") (current-digimon))])
             (list (test-suite "Rewrite URL" |<testcase: rewrite url>|)
                   |<testcase: basic access authentication>|)))}]

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
