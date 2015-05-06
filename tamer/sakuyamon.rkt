#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Hello, Sakuyamon!}

As the @deftech{digimon tamer}s, our story always starts with checking the @deftech{digivice}
in order to make sure we could talk with the @deftech{digimon}s as expected.

@margin-note{The basic testing on @itech{digivice} has already performed
                                  @hyperlink[(format "http://~a.gyoudmon.org/digivice.rkt" (digimon-gnome))]{here}.}

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
       (let*-values ([{shutdown sendrecv} (sakuyamon-realize "-p" "8080")]
                     [{shutdown-errno recv-stdmsg} (sakuyamon-realize "-p" "8080")]
                     [{shutdown resendrecv} (and (shutdown) (sakuyamon-realize "-p" "8080"))])
         (test-spec "realize --port 8080 [fresh]"
                    (let ([$? (shutdown)])
                      (check-pred procedure? sendrecv)
                      (check-pred procedure? resendrecv)
                      (check-pred zero? $?)))
         (test-spec "realize --port 8080 [already in use]"
                    (let ([$? (shutdown-errno)])
                      (check-pred pair? recv-stdmsg)
                      (check-regexp-match (pregexp (format "errno=~a" $?)) (cdr recv-stdmsg)))))]

@subsection{Dispatch Rules}

The @bold{Racket} @deftech{Web Server} is just a configuration of a dispatching server,
and @itech{Sakuyamon} redefines all the default dispatchers to match the @itech[#:key "Terminus"]{Termini}.

By default, @itech{Per-User Terminus} and @itech{Per-Digimon Terminus} are disabled
since they are system wide @itech[#:key "Terminus"]{Termini}.

@handbook-rule{The first two @italic{path element}s of the @racket[url] are used to detect the types of @itech[#:key "Terminus"]{termini}.
                             All @itech[#:key "Terminus"]{termini} except @itech{Per-User Terminus}
                             and @itech{Per-Digimon Terminus} are the @itech{Main Terminus}.}

@chunk[|<testcase: request-main>|
       (for/list ([path (in-list (list "/" "/~" "/wargrey/.sakuyamon" "/.sakuyamon/~wargrey"))])
         (test-case path (let-values ([{status headers /dev/net/stdin} (sendrecv path)])
                           |<check: dispatch-!5xx>|
                           (check-equal? #"Main" (ormap (curry extract-field #"Terminus") headers)))))]

@handbook-rule{The @itech{Per-User Terminus} is the one whose first @italic{path element} starts with @litchar{~} followed by valid chars.}

@chunk[|<testcase: request-user>|
       (for/list ([path (in-list (list "/~wargrey" "/~bin/." "/~root/default.rkt"))])
         (test-case path (let-values ([{status headers /dev/net/stdin} (sendrecv path)])
                           |<check: dispatch-!5xx>|
                           (check-equal? #"Per-User" (ormap (curry extract-field #"Terminus") headers)))))]

@handbook-rule{The @itech{Per-Digimon Terminus} is the one within a @itech{Per-User Terminus}
                   whose second @italic{path element} starts with @litchar{.} followed by valid chars.}

@chunk[|<testcase: request-digimon>|
       (for/list ([path (in-list (list "/~bin/.sakuyamon" "/~nobody/.DigiGnome/index.html"))])
         (test-case path (let-values ([{status headers /dev/net/stdin} (sendrecv path)])
                           |<check: dispatch-!5xx>|
                           (check-equal? #"Per-Digimon" (ormap (curry extract-field #"Terminus") headers)))))]

@handbook-appendix[]

@chunk[|<sakuyamon:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite realize "Sakuyamon, Realize!"
           |<testcase: realize>|
           (let-values ([{shutdown sendrecv} (sakuyamon-realize)])
             (test-suite "Dispatch Rules!"
                         #:before {λ _ (when (pair? sendrecv)
                                         (raise-result-error 'realize "procedure?" sendrecv))}
                         #:after {λ _ (shutdown)}
                         (test-suite "Main Terminus" |<testcase: request-main>|)
                         (test-suite "Per-User Terminus" |<testcase: request-user>|)
                         (test-suite "Per-Digimon Terminus" |<testcase: request-digimon>|))))}]

@chunk[|<check: dispatch-!5xx>|
       (check-regexp-match #px"^HTTP.+?\\s+[^5]\\d{2}\\s+"
                           (bytes->string/utf-8 status)
                           (port->string /dev/net/stdin))]
