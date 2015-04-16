#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@(tamer-story (tamer-story->libpath "sakuyamon.rkt"))
@(define partner (tamer-partner->libpath "digivice/sakuyamon.rkt"))
@(tamer-zone (make-tamer-zone))

@handbook-story{Hello, Sakuyamon!}

As the @deftech{digimon tamer}s, our story always starts with checking the @deftech{digivice}
in order to make sure we could talk with the @deftech{digimon}s as expected.

The basic testing on @itech{digivice} has already performed
@hyperlink[(format "http://~a.gyoudmon.org/digivice.rkt" (digimon-gnome))]{here},
hence we only need to recall the @deftech{action}s:

@tamer-action[(parameterize ([current-command-line-arguments (vector)]
                             [exit-handler void])
                ((dynamic-require/expose (tamer-story) 'sakuyamon)))]

@tamer-smart-summary[]

@chunk[|<digivice taming start>|
       (require "tamer.rkt")
       
       (tamer-story (tamer-story->libpath "sakuyamon.rkt"))
       (define partner (tamer-partner->libpath "digivice/sakuyamon.rkt"))
       (define sakuyamon (parameterize ([current-command-line-arguments (vector)]
                                        [current-output-port /dev/null]
                                        [exit-handler void])
                           (dynamic-require/expose partner 'main)))

       |<digivice:*>|]

@handbook-scenario{Sakuyamon, Realize!}

@chunk[|<sakuyamon, realize!>|
       (define-values {outin stdout} (make-pipe #false 'outin 'stdout))
       (define-values {errin stderr} (make-pipe #false 'errin 'stderr))
       
       (define-tamer-case realize "Sakuyamon, Realize!" |<testcase: realize>|)]

Technically, @itech{Sakuyamon} is hard to cooperate with by simulating a real world.
For the sake of simplicity, all optional parameters are defined in
@hyperlink[(build-path (digimon-zone) "info.rkt")]{@filepath{info.rkt}}.

@margin-note{Meanwhile I do not care about HTTPS since HTTP and its age of plaintext transmission
             is almost over. @hyperlink["https://letsencrypt.org"]{Let@literal{'}s Encrypt} is a kind of
             service that allow network administrator enabling HTTPS esaily, freely and automatically.}

@tamer-action[(define info-ref (get-info/full (digimon-zone)))
              (info-ref 'sakuyamon-config-ssl {λ _ #false})
              (info-ref 'sakuyamon-config-port {λ _ #false})
              (parameterize ([exit-handler void])
                ((dynamic-require/expose (tamer-story) 'sakuyamon) "realize" "--help"))]

Once @italic{sakuyamon} has realized she would keep doing her duty,
however this should be stopped in the current situation;
otherwise she should tell us what is wrong.

@tamer-note['realize]

@chunk[|<testcase: realize>|
       (define send-status (curry thread-send (current-thread)))
       (define taming (thread {λ _ (parameterize ([current-output-port stdout]
                                                  [current-error-port stderr]
                                                  [exit-handler send-status])
                                     (sakuyamon "realize"))}))
       (define which (sync/timeout 0.618 outin (thread-receive-evt)))
       (for-each close-output-port (list stdout stderr))
       (define errmsg (if which (port->string errin) "sakuyamon is delayed."))
       (cond [(eq? which outin) (check-pred void? (break-thread taming 'terminate))]
             [(regexp-match #px"(?<=errno=)\\d+" errmsg)
              => {λ [eno] (check = (thread-receive) (string->number (car eno)))}]
             [else (fail errmsg)])]

@handbook-appendix[]

@chunk[|<digivice:*>|
       {module+ story
         |<sakuyamon, realize!>|}

       {module+ main (call-as-normal-termination tamer-prove)}]
