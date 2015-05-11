#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Hello, Sakuyamon!}

As the @deftech{digimon tamer}s, our story always starts with checking the @deftech{digivice}
in order to make sure we could talk with the @deftech{digimon}s as expected.

@tamer-smart-summary[]

@chunk[|<sakuyamon taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       |<sakuyamon:*>|]

@handbook-scenario{Sakuyamon, Realize!}

Once @itech{sakuyamon} has realized she would keep doing her duty before @racketidfont{shutdown}ing manually.
All the options are designed for taming rather than real world surviving, so the default port is @racket[0]
which means she can always be talked with via @racketidfont{sendrecv} as long as she@literal{'}s ready.

@tamer-action[(define {realize-with-flush . arglist}
                (call-with-values {λ _ (apply sakuyamon-realize arglist)}
                                  {λ [shutdown sendrecv]
                                    (let ([$? (shutdown)])
                                      (when (pair? sendrecv)
                                        (cond [(zero? $?) (printf "~a~n" (car sendrecv))]
                                              [else (eprintf "~a~n" (cdr sendrecv))])))}))
              (code:comment @#,t{@racketidfont{@racketcommentfont{sendrecv}} will hold the output and error messages.})
              (realize-with-flush "--help")
              (realize-with-flush "--SSL")
              (code:comment @#,t{@hyperlink["https://letsencrypt.org"]{@racketcommentfont{Let@literal{'}s Encrypt}} is a kind of service})
              (code:comment @#,t{that allow administrator enabling HTTPS esaily, freely and automatically.})]

So, as usual @racket[sakuyamon-realize] itself should be checked first:

@tamer-note['realize]

@chunk[|<testcase: realize>|
       (let*-values ([{shutdown sendrecv} (sakuyamon-realize "-p" "8443")]
                     [{shutdown-errno recv-stdmsg} (sakuyamon-realize "-p" "8443")]
                     [{shutdown resendrecv} (and (shutdown) (sakuyamon-realize "-p" "8443"))])
         (test-spec "realize --port 8443 [fresh]"
                    (let ([$? (shutdown)])
                      (check-pred procedure? sendrecv)
                      (check-pred procedure? resendrecv)
                      (check-pred zero? $?)))
         (test-spec "realize --port 8443 [already in use]"
                    (let ([$? (shutdown-errno)])
                      (check-pred pair? recv-stdmsg)
                      (check-regexp-match (pregexp (format "errno=~a" $?)) (cdr recv-stdmsg)))))]

@handbook-appendix[]

@chunk[|<sakuyamon:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite realize "Sakuyamon, Realize!" |<testcase: realize>|)}]
