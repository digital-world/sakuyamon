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

@margin-note{For the two @hyperlink["http://en.wikipedia.org/wiki/Localhost"]{loopback addresses},
                    @itech{Sakuyamon} always trust @deftech[#:key "trustable"]@litchar{::1} and treat
                    @litchar{127.0.0.1} as a public address.}

@chunk[|<dispatch taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (define htdocs (curry format "/~a"))
       (define ~user (curry format "/~~~a/~a" (getenv "USER")))
       (define .digimon (curry format "/~~~a/.~a/~a" (getenv "USER") (current-digimon)))

       (define /htdocs (curry build-path (digimon-terminus)))
       (define /user (curry build-path (find-system-path 'home-dir) "Public/DigitalWorld" "terminus"))
       (define /digimon (curry build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook"))
       
       (define-values {shutdown curl} (sakuyamon-realize))
       (define {127.curl uri #:method [method #"GET"] #:headers [headers null] #:data [data #false]}
         (curl uri #:host "127.0.0.1" #:method method #:headers headers #:data data))
       
       |<dispatch:*>|]

@handbook-scenario{Main Terminus}

@deftech{Main Terminus} is the major one shipped with @itech{Sakuyamon},
all URL paths other than ones of @itech{Per-User Terminus} and @itech{Per-Digimon Terminus}
are relative to @racket[digimon-terminus].

@tamer-note['dispatch-main]

@chunk[|<testcase: dispatch main>|
       (for ([path (in-list (list "trick/.digimon/readme.t"))])
         (let-values ([{lpath rpath} (values (/htdocs path) (htdocs path))])
           (test-spec rpath |<dispatch: setup and teardown>| |<check: dispatch>|)))]

@chunk[|<check: dispatch>|
       (match-let ([{list status reason _ /dev/net/stdin} (curl rpath)])
         (check-eq? status 200 reason)
         (check-equal? (read-line /dev/net/stdin) (path->string lpath)))]

@deftech{Function URL}s are dispatched only when the requests come from @itech{trustable} clients.

@chunk[|<testcase: dispatch funtion URLs>|
       (for ([d-arc (in-list (list "d-arc/collect-garbage" "d-arc/refresh-servlet"))])
         (let ([~htdocs htdocs]) |<check: function url>|))]

@chunk[|<check: function url>|
       (test-case (format "200: ~a@::1" (~htdocs d-arc))
                  (match-let ([{list status reason _ _} (curl (~htdocs d-arc))])
                    (check-eq? status 200 reason)))
       (test-case (format "403: ~a@127" (~htdocs d-arc))
                  (match-let ([{list status reason _ _} (127.curl (~htdocs d-arc))])
                    (check-eq? status 403 reason)))]

@handbook-scenario{Per-User Terminus}

@deftech{Per-User Terminus} is designed for system users to share and discuss their works on the internet
if they store contents in the directory @litchar{$HOME/Public/DigitalWorld} and organise them as a
@hyperlink["https://github.com/digital-world/DigiGnome"]{digimon}. URL paths always start with @litchar{~user}
and the rest parts are relative to @racket[digimon-terminus].

@tamer-note['dispatch-user]

@chunk[|<testcase: dispatch user>|
       (for ([path (in-list (list "readme.t"))])
         (let-values ([{lpath rpath} (values (/user path) (~user path))]) 
           (test-spec rpath |<dispatch: setup and teardown>| |<check: dispatch>|)))]

Note that @itech{Per-User Terminus} do support @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")].
So users should be responsible for their own @itech{function URL}s.

@chunk[|<testcase: dispatch-user-funtion-URLs>|
       (for ([d-arc (in-list (list "d-arc/refresh-servlet"))])
         (let ([~htdocs ~user]) |<check: function url>|))]

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
         (let-values ([{lpath rpath} (values (/digimon path) (.digimon path))])
           (test-spec rpath |<dispatch: setup and teardown>| |<check: dispatch>|)))]

@itech{Per-Digimon Terminus} only serves static contents that usually be generated from the
@secref["scribble_lp2_Language" #:doc '(lib "scribblings/scribble/scribble.scrbl")].
So paths reference to any @litchar{*.rktl} will be redirected to their @litchar{*.html} counterparts.
Here @litchar{l} stands for @secref["lp" #:doc '(lib "scribblings/scribble/scribble.scrbl")] 
rather than its original intention: @secref["load-lang" #:doc '(lib "scribblings/reference/reference.scrbl")].

Nonetheless, these paths would always be navigated by auto-generated navigators. All we need do is
making sure it works properly.

@chunk[|<testcase: rewrite url>|
       (for ([rpath (in-list (list "!/../." "./t/h.lp.rktl" "../../tamer.rkt"))]
             [px (in-list (list #px"/$" #px"t_h_lp_rktl(/|\\.html)$" #false))]
             [expect (in-list (list 302 302 418))])
         (make-directory* (/digimon))
         (test-case (format "~a: ~a" expect rpath)
                    (match-let ([{list status reason headers _} (curl (.digimon rpath))])
                      (check-eq? status expect reason)
                      (when (and (= expect 302) (regexp? px))
                        (let ([location (dict-ref headers #"Location")])
                          (check-regexp-match px (bytes->string/utf-8 location)))))))]

Sometimes, users may want to hide their private projects, although this is not recommended.
Nonetheless, @itech{Per-Digimon Terminus} do support
@hyperlink["http://en.wikipedia.org/wiki/Basic_access_authentication"]{HTTP Basic Access Authentication}.
Just put a @racket[read]able @italic{data file} @litchar{realm.rktd} in the @racket[digimon-tamer] to work with
@secref["dispatch-passwords" #:doc '(lib "web-server/scribblings/web-server-internal.scrbl")].

@chunk[|<testcase: basic access authentication>|
       (let* ([realm.rktd (build-path (digimon-tamer) "realm.rktd")]
              [realm (path->string (file-name-from-path realm.rktd))]
              [client {λ [curl . lines]
                        (let ([>> {λ _ (map displayln lines)}]
                              [<< {λ _ (sakuyamon-agent curl (.digimon realm) #"GET")}])
                          (with-input-from-bytes (with-output-to-bytes >>) <<))}])
         (test-suite "Basic Authentication"
                     #:before {λ _ (void (with-output-to-file realm.rktd #:exists 'error
                                           {λ _ (printf "'~s~n" `{{"realm" ,(regexp-quote realm)
                                                                           [user "password"]
                                                                           [tamer "opensource"]}})})
                                         (make-directory* (/digimon))
                                         (copy-file realm.rktd (/digimon realm)))}
                     #:after {λ _ (void (delete-file (/digimon realm))
                                        (delete-file realm.rktd))}
                     (test-case "200: guest@::1"
                                (match-let ([{list status reason _ _} (client curl)])
                                  (check-eq? status 200 reason)))
                     (test-case "401: guest@127"
                                (match-let ([{list status reason _ _} (client 127.curl)])
                                  (check-eq? status 401 reason)))
                     (test-case "200: tamer@127"
                                (let ([curl:user:pwd (list 127.curl 'tamer "opensource")])
                                  (match-let ([{list status reason _ _} (apply client curl:user:pwd)])
                                    (check-eq? status 200 reason))))))]

By the way, as you may guess, users don@literal{'}t need to refresh passwords manually
since the @litchar{realm.rktd} is checked every request.

@handbook-appendix[]

@chunk[|<dispatch:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite dispatch-main "Main Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch main>|
           (test-suite "Function URLs" |<testcase: dispatch funtion URLs>|))

         (define-tamer-suite dispatch-user "Per-User Terminus"
           |<dispatch: check #:before>|
           |<testcase: dispatch user>|
           (test-suite "Function URLs" |<testcase: dispatch-user-funtion-URLs>|))

         (define-tamer-suite dispatch-digimon "Per-Digimon Terminus"
           |<dispatch: check #:before>| #:after {λ _ (shutdown)}
           |<testcase: dispatch digimon>|
           (list (test-suite "Rewrite URL" |<testcase: rewrite url>|)
                 |<testcase: basic access authentication>|))}]

@chunk[|<dispatch: check #:before>|
       #:before {λ _ (when (pair? curl) (raise-result-error 'realize "procedure?" curl))}]

@chunk[|<dispatch: setup and teardown>|
       #:before {λ _ (void (make-parent-directory* lpath)
                           (display-to-file #:exists 'replace lpath lpath))}
       #:after {λ _ (void (delete-file lpath)
                          (with-handlers ([exn? void])
                            (let rmdir ([dir (path-only lpath)])
                              (delete-directory dir)
                              (rmdir (build-path dir 'up)))))}]
