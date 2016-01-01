#lang scribble/lp2

@(require "tamer.rkt")

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

       (module+ tamer |<sakuyamon:*>|)]

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "help"))]

@handbook-scenario{Sakuyamon, Realize!}

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "realize" "--help"))]

@itech{Sakuyamon} herself is designed as a daemon, hence the taming strategy is following this fact.
If the @itech{tamer} is @italic{root}, she will not realize automatically since she should have already been deloyed
and listen on port @racket[80], otherwise she will realize and listen on port @racket[16180] during the taming process.

@tamer-note['realize]
@chunk[|<testcase: realize>|
       (test-spec "sakuyamon realize"
                  (check-not-exn (check-port-ready? tamer-sakuyamon-port #:type error)))]
 
@handbook-scenario{Keep Realms Safe!}

Apart from heavy-weight authentication solutions implemented by website developers on their own,
HTTP protocal has two alternatives, the
@deftech[#:key "BAA"]{@hyperlink["http://en.wikipedia.org/wiki/Basic_access_authentication"]{Basic Access Authentication}} and
@deftech[#:key "DAA"]{@hyperlink["http://en.wikipedia.org/wiki/Digest_access_authentication"]{Digest Access Authentication}}.
As lightweight as they are, the only requirement is a @racket[read]able data file @deftech{.realm.rktd}.

@racketcommentfont{See @itech{Per-Digimon Terminus} and @itech{Per-Tamer Terminus} to check how @itech{Sakuyamon} applies it.}

@tamer-racketbox[(build-path (digimon-stone) "realm.rktd")]

Like @hyperlink["http://en.wikipedia.org/wiki/Digest_access_authentication#The_.htdigest_file"]{@exec{htdigest}},
@itech{Sakuyamon} has a tool @exec{sphere} to help users to digest their flat @itech{.realm.rktd}s.

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "sphere" "--help"))
              (parameterize ([exit-handler void])
                (sakuyamon "sphere" realm.rktd))]

Note that @exec{sphere} will do nothing for those passwords that have already been updated.

@tamer-note['sphere]
@chunk[|<testcsae: sphere in-place>|
       (test-spec "sakuyamon sphere --in-place"
                  #:before (let ([realm.dtkr (path->string (make-temporary-file "~a.dtkr" realm.rktd))])
                             (thunk ($shell (thunk (sakuyamon "sphere" realm.dtkr)
                                                   (sakuyamon "sphere" "--in-place" realm.dtkr)
                                                   (sakuyamon "sphere" realm.dtkr)))))
                  (parameterize ([current-input-port (open-input-bytes (get-output-bytes $out))])
                    (check-pred zero? ($?) (get-output-string $err))
                    (check-equal? (read) (read))))]

@handbook-scenario{How is everything going?}

Logging plays an important role in the lifecycle of Web Application, but the fact is that
most modern operating systems such as Unixes do not provide a good out of box tool
to monitor the logs just as MacOSX @exec{Console.app} does. Therefore here exists
@exec{foxpipe} and @exec{izuna}.

@tamer-action[(parameterize ([exit-handler void])
                (sakuyamon "foxpipe" "--help"))
              (parameterize ([exit-handler void])
                (sakuyamon "izuna" "--help"))]

@tamer-note['foxpipe]
@chunk[|<testcsae: foxpipe>|
       (test-spec "sakuyamon foxpipe"
                  (check-not-exn (check-port-ready? tamer-foxpipe-port #:type skip)))]

@handbook-bibliography[]

@chunk[|<sakuyamon:*>|
       (module+ story
         (define-tamer-suite realize "Sakuyamon, Realize!" |<testcase: realize>|)
         (define-tamer-suite sphere "Keep Realms Safe!" |<testcsae: sphere in-place>|)
         (define-tamer-suite foxpipe "How is everything going?" |<testcsae: foxpipe>|))]
