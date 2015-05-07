#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Dispatching Rules!}

As an instance of the @bold{Racket} @deftech{Web Server},
@itech{Sakuyamon} is just a configuration of a dispatching server
that serves 3 types of @deftech[#:key "terminus"]{termini}, or @italic{htdocs}.

By default, @itech{Per-User Terminus} and @itech{Per-Digimon Terminus} are disabled
since they are system-wide @itech[#:key "Terminus"]{Termini}.

@tamer-smart-summary[]

@chunk[|<dispatch taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       (define-values {shutdown sendrecv} (sakuyamon-realize))
       |<dispatch:*>|]

@handbook-scenario{Main Terminus}

@deftech{Main Terminus} is the major one shipped with @itech{Sakuyamon},
all URL paths other than ones of @itech{Per-User Terminus} and @itech{Per-Digimon Terminus}
are relative to @racket[digimon-terminus].

@tamer-note['dispatch-main]

@chunk[|<testcase: dispatch-main>|
       (for/list ([path (in-list (list "/" "/~" "/error.css" "/user/.digimon" "/.digimon/~user"))])
         (test-case path (let-values ([{status headers /dev/net/stdin} (sendrecv path)])
                           |<check: dispatch-!5xx>|
                           (check-equal? #"Main" |<extract terminus>|))))]

For the sake of security, these @deftech{function URL}s are dispatched only when the client is @litchar{::1}.

@chunk[|<testcase: dispatch-funtion-URLs>|
       (let ([gc "/conf/collect-garbage"])
         (test-case (format "[::1]~a" gc)
                    (let-values ([{status headers /dev/net/stdin} (sendrecv gc)])
                      (check-regexp-match #px"^.+?\\s+200\\s+" (bytes->string/utf-8 status))
                      (check-regexp-match #px".+?MB = .+?MB - .+?MB" (port->string /dev/net/stdin))))
         (test-case (format "[127.0.0.1]~a" gc)
                    (let-values ([{status headers /dev/net/stdin} (sendrecv gc #:host "127.0.0.1")])
                      (check-regexp-match #px"^.+?\\s+404\\s+" (bytes->string/utf-8 status)))))]

@handbook-scenario{Per-User Terminus}

@deftech{Per-User Terminus} is designed for system users to share and discuss their works on the internet
if they store the content in the directory @litchar{$HOME/Public/DigitalWorld}.
URL paths always start with @litchar{~username}.

@tamer-note['dispatch-user]

@chunk[|<testcase: dispatch-user>|
       (for/list ([path (in-list (list "/~user" "/~user/."))])
         (test-case path (let-values ([{status headers /dev/net/stdin} (sendrecv path)])
                           |<check: dispatch-!5xx>|
                           (check-equal? #"Per-User" |<extract terminus>|))))]

@handbook-scenario{Per-Digimon Terminus}

@deftech{@bold{Per-Digimon Terminus}} is designed for system users to publish their project wikis like
@hyperlink["https://help.github.com/articles/what-are-github-pages/"]{Github Pages}. Public projects
should be stored in directory @litchar{$HOME/DigitalWorld}, and URL paths always start with
@litchar{~username} followed by the @italic{path element} @litchar{.projectname}.

@tamer-note['dispatch-digimon]

@chunk[|<testcase: dispatch-digimon>|
       (for/list ([path (in-list (list "/~user/.digimon"))])
         (test-case path (let-values ([{status headers /dev/net/stdin} (sendrecv path)])
                           |<check: dispatch-!5xx>|
                           (check-equal? #"Per-Digimon" |<extract terminus>|))))]

@handbook-appendix[]

@chunk[|<dispatch:*>|
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite dispatch-main "Main Terminus"
           |<check: before>|
           |<testcase: dispatch-main>|
           (test-suite "Function URLs" |<testcase: dispatch-funtion-URLs>|))

         (define-tamer-suite dispatch-user "Per-User Terminus"
           |<check: before>|
           |<testcase: dispatch-user>|)

         (define-tamer-suite dispatch-digimon "Per-Digimon Terminus"
           |<check: before>| #:after {λ _ (shutdown)}
           |<testcase: dispatch-digimon>|)}]

@chunk[|<check: before>|
       #:before {λ _ (when (pair? sendrecv) (raise-result-error 'realize "procedure?" sendrecv))}]

@chunk[|<check: dispatch-!5xx>|
       (check-regexp-match #px"^HTTP.+?\\s+[^5]\\d{2}\\s+"
                           (bytes->string/utf-8 status)
                           (port->string /dev/net/stdin))]

@chunk[|<extract terminus>|
       (ormap (curry extract-field #"Terminus") headers)]
