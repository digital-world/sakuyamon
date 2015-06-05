#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Hello, Sakuyamon!}

As the @deftech{digimon} @deftech{tamer}s, our story always starts with checking the
@deftech{@hyperlink["http://gyoudmon.org/~wargrey:DigiGnome/digivice.rkt"]{digivice}}
in order to make sure we could talk with the @itech{digimon}s as expected.

@tamer-smart-summary[]

@chunk[|<sakuyamon taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (define sakuyamon
         (parameterize ([current-command-line-arguments (vector)]
                        [current-output-port /dev/null]
                        [current-error-port /dev/null]
                        [exit-handler void])
           (dynamic-require (build-path (digimon-digivice) "sakuyamon.rkt") 'main)))
       
       |<sakuyamon:*>|]

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "help"))]

@handbook-scenario{Sakuyamon, Realize!}

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "realize" "--help"))
              (parameterize ([exit-handler void])
                (sakuyamon "realize" "--SSL"))
              (code:comment @#,t{@hyperlink["https://letsencrypt.org"]{@racketcommentfont{Let@literal{'}s Encrypt}} is a kind of service})
              (code:comment @#,t{that allow administrator enabling HTTPS esaily, freely and automatically.})]

@itech{Sakuyamon} herself is designed as a daemon, hence the taming strategy is following this fact.
If the @itech{tamer} is @italic{root}, she will not realize automatically since she should have already been deloyed
and listen on port @racket[80], otherwise she will realize and listen on port @racket[16180] during the taming process.

@tamer-note['realize]
@chunk[|<testcase: realize>|
       (test-spec "realize?" (check-not-exn (check-ready? 'realize)))]
 
@handbook-scenario{Keep Realms Safe!}

Apart from heavy-weight authentication solutions implemented by website developers on their own,
HTTP protocal has two alternatives, the
@deftech[#:key "BAA"]{@hyperlink["http://en.wikipedia.org/wiki/Basic_access_authentication"]{Basic Access Authentication}} and
@deftech[#:key "DAA"]{@hyperlink["http://en.wikipedia.org/wiki/Digest_access_authentication"]{Digest Access Authentication}}.
As lightweight as they are, the only requirement is a @racket[read]able data file @deftech{.realm.rktd}.

@para[#:style "GYDMComment"]{See @itech{Per-Digimon Terminus} and @itech{Per-Tamer Terminus}
                                 to check how @itech{Sakuyamon} applies it.}

@tamer-racketbox[#:line-start-with 1 (build-path (digimon-stone) "realm.rktd")]

Like @hyperlink["http://en.wikipedia.org/wiki/Digest_access_authentication#The_.htdigest_file"]{@exec{htdigest}},
@itech{Sakuyamon} has a tool @exec{realm} to help users to digest their flat @itech{.realm.rktd}s.

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "realm" "--help"))
              (parameterize ([exit-handler void])
                (sakuyamon "realm" realm.rktd))]

Note that @exec{realm} will do nothing for those passwords that have already been updated.

@tamer-note['realm]
@chunk[|<testcsae: realm in-place>|
       (let-values ([{realm.dtkr} (path->string (path-replace-suffix realm.rktd ".dtkr"))]
                    [{digest-in digest-out} (make-pipe #false 'digest-in 'digest-out)])
         (test-spec "realm --in-place"
                    #:before (thunk (copy-file realm.rktd realm.dtkr))
                    #:after (thunk (delete-file realm.dtkr))
                    (parameterize ([current-output-port digest-out]
                                   [current-error-port digest-out])
                      (check-equal? (parameterize ([exit-handler void])
                                      (thread (thunk (sakuyamon "realm" realm.dtkr)))
                                      (read digest-in))
                                    (parameterize ([exit-handler void])
                                      (thread (thunk (and (sakuyamon "realm" "--in-place" realm.dtkr)
                                                          (sakuyamon "realm" realm.dtkr))))
                                      (read digest-in))))))]

@handbook-appendix[]

@chunk[|<sakuyamon:*>|
       (module+ main (call-as-normal-termination tamer-prove))
       (module+ story
         (define-tamer-suite realize "Sakuyamon, Realize!" |<testcase: realize>|)
         (define-tamer-suite realm "Keep Realms Safe!" |<testcsae: realm in-place>|))]
