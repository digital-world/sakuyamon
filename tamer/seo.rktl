#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Search Engine Optimization}

As a beginning practitioner of @deftech{@hyperlink["http://en.wikipedia.org/wiki/Search_engine_optimization"]{SEO}},
I follows Google@literal{'}s guides.

Meanwhile, @hyperlink["http://en.wikipedia.org/wiki/Webserver_directory_index"]{directory indices}
are @litchar{default.rkt} and @litchar{index.html}, respectively.

@tamer-smart-summary[]

@chunk[|<seo taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       (define-values {shutdown curl 127.curl} (sakuyamon-realize))
       |<seo:*>|]

@handbook-scenario{Robots Exclusion Protocol}

@para[#:style "GYDMComment"]{Just a placeholder for the future if @litchar{robots.txt} really need some rules.}

@handbook-scenario{Server Side Redirection}

@tamer-note['redirections]

@handbook-rule{All non-/-suffixed directories should be redirected to their /-suffixed counterparts.}

@chunk[|<testcase: dir to dir/>|
       (for ([rpath (in-list (list "."))])
         (test-case (format "302: ~a" rpath)
                    (match-let ([{list status reason headers _} (curl (/htdocs rpath))])
                      (check-eq? status 302 reason)
                      (check-regexp-match #px"/$" (dict-ref headers 'location)))))]

@handbook-rule{Shortcuts of @litchar{~} and @litchar{:} should be redirected to their full shapes with /-suffixed.}

For @itech{Per-Tamer Terminus} and @itech{Per-Digimon Terminus}, @litchar{~username:digimon} has lots of variables for short.
In practice these shortcuts always make nonsense since @itech{Sakuyamon} will be deployed and run as a non-root user
who has a fresh @litchar{$HOME}. But nonetheless, she is also helpful working in a local machine.

Of course the default username is yourself while the default digimon is @litchar{Kuzuhamon}.

@chunk[|<testcase: expand ~:>|
       (test-case "302: ~"
                  (match-let ([{list status reason headers _} (curl (/htdocs "~"))])
                    (check-eq? status 302 reason)
                    (check-regexp-match #px"^/(~.+?)/$" (dict-ref headers 'location))))
       (for ([rpath (in-list (list ":" "~:" (~a ":" (current-digimon)) (~a "~" (getenv "USER") ":")))])
         (test-case (format "302: ~a" rpath)
                    (match-let ([{list status reason headers _} (curl (/htdocs rpath))])
                      (check-eq? status 302 reason)
                      (check-regexp-match #px"^/(~.+?):(.+?)/$" (dict-ref headers 'location)))))]

@handbook-rule{Paths reference to any @litchar{*.rktl} should be redirected to their @litchar{*.html} counterparts.}

@para[#:style "GYDMComment"]{Note @litchar{.rktl} stands for
                                  @secref["lp" #:doc '(lib "scribblings/scribble/scribble.scrbl")] 
                                  rather than its original intention:
                                  @secref["load-lang" #:doc '(lib "scribblings/reference/reference.scrbl")].}

@itech{Terminus} like @itech{Per-Digimon Terminus} serves static contents that usually be auto-generated by @bold{Scribble}.
The rendered @litchar{*.html}s will be placed within directories that up to 2 depths, thereforce the path parts of
@litchar{*.rktl} if exist should be fixed either.

@chunk[|<testcase: rktl to html>|
       (for ([rpath (in-list (list "./t/h.lp.rktl"))]
             [px (in-list (list #px"t_h_lp_rktl(/|\\.html)$"))])
         (test-case (format "302: ~a" rpath)
                    (match-let ([{list status reason headers _} (curl (/digimon rpath))])
                      (check-eq? status 302 reason)
                      (check-regexp-match px (dict-ref headers 'location)))))]

@handbook-appendix{SEO Auxiliaries}

@chunk[|<seo:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite redirections "Server Side Redirections"
           |<seo: check #:before>| #:after {λ _ (shutdown)}
           (test-suite "dir -> dir/" |<testcase: dir to dir/>|)
           (test-suite "expand ~:" |<testcase: expand ~:>|)
           (test-suite "rktl -> html" |<testcase: rktl to html>|))}]

@chunk[|<seo: check #:before>|
       #:before {λ _ (when (string? 127.curl) (raise-result-error 'realize "procedure?" 127.curl))}]
