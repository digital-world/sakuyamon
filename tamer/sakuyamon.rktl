#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Hello, Sakuyamon!}

As the @deftech{digimon} @deftech{tamer}s, our story always starts with checking the
@deftech{@hyperlink["http://gyoudmon.org/~wargrey:DigiGnome/digivice.rktl"]{digivice}}
in order to make sure we could talk with the @itech{digimon}s as expected.

@tamer-smart-summary[]

@chunk[|<sakuyamon taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (define sakuyamon (parameterize ([current-command-line-arguments (vector)]
                                        [current-output-port /dev/null]
                                        [current-error-port /dev/null]
                                        [exit-handler void])
                           (dynamic-require (build-path (digimon-digivice) "sakuyamon.rkt") 'main)))
       
       |<sakuyamon:*>|]

@handbook-scenario{Sakuyamon, Realize!}

Once @itech{sakuyamon} has realized she would keep doing her duty before @racketidfont{shutdown}ing manually.
All the options are designed for taming rather than real world surviving, so the default port is @racket[0]
which means she can always be talked with via @racketidfont{curl} as long as she@literal{'}s ready.

@tamer-action[(define {realize-with-flush . arglist}
                (call-with-values {λ _ (apply sakuyamon-realize arglist)}
                                  {λ [shutdown /maybe/stdout /maybe/stderr]
                                    (let ([$? (shutdown)])
                                      (when (string? /maybe/stdout)
                                        (cond [(zero? $?) (printf "~a~n" /maybe/stdout)]
                                              [else (eprintf "~a~n" /maybe/stderr)])))}))
              (realize-with-flush "--help")
              (realize-with-flush "--SSL")
              (code:comment @#,t{@hyperlink["https://letsencrypt.org"]{@racketcommentfont{Let@literal{'}s Encrypt}} is a kind of service})
              (code:comment @#,t{that allow administrator enabling HTTPS esaily, freely and automatically.})]

As usual @racket[sakuyamon-realize] itself should be checked first:

@tamer-note['realize]
@chunk[|<testcase: realize>|
       (let*-values ([{shutdown ::1.sendrecv 127.sendrecv} (sakuyamon-realize "-p" "8443")]
                     [{shutdown=>errno stdout stderr} (sakuyamon-realize "-p" "8443")]
                     [{shutdown ::1.curl 127.curl} (and (shutdown) (sakuyamon-realize "-p" "8443"))])
         (test-spec "realize --port 8443 [fresh]"
                    (let ([$? (shutdown)])
                      (check-pred procedure? 127.sendrecv)
                      (check-pred procedure? 127.curl)
                      (check-pred zero? $?)))
         (test-spec "realize --port 8443 [already in use]"
                    (let ([$? (shutdown=>errno)])
                      (check-pred string? stderr)
                      (check-pred (curryr member '{48 125}) $? stderr))))]

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
                    #:before {λ _ (copy-file realm.rktd realm.dtkr)}
                    #:after {λ _ (delete-file realm.dtkr)}
                    (parameterize ([current-output-port digest-out]
                                   [current-error-port digest-out])
                      (check-equal? (parameterize ([exit-handler void])
                                      (thread {λ _ (sakuyamon "realm" realm.dtkr)})
                                      (read digest-in))
                                    (parameterize ([exit-handler void])
                                      (thread {λ _ (and (sakuyamon "realm" "--in-place" realm.dtkr)
                                                        (sakuyamon "realm" realm.dtkr))})
                                      (read digest-in))))))]

@handbook-appendix[]

@chunk[|<sakuyamon:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite realize "Sakuyamon, Realize!" |<testcase: realize>|)
         (define-tamer-suite realm "Keep Realms Safe!" |<testcsae: realm in-place>|)}]
