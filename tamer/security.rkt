#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{Bad Guys}

In practice, there is no comprehensive solution for security problems,
nonetheless, we can always choose to make things interesting.

@tamer-smart-summary[]

@chunk[|<security taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       (module+ tamer |<security:*>|)]

@handbook-scenario{Would you like a cup of tea?}

This is, or is not, an Apirl Fools@literal{'} joke.
If @tech{@#%digimon} thinks you are a bad guy, she will send you
@hyperlink["http://en.wikipedia.org/wiki/Hyper_Text_Coffee_Pot_Control_Protocol"]{a cup of tea},
then enjoy it yourself!

@tamer-note['I-am-a-teapot]

@chunk[|<testcase: I-am-a-teapot>|
       (for ([rpath (in-list (list "../tamer/.realm.rktd"))])
         (test-case (format "418: ~a" rpath)
                    (match-let ([{list status reason _ _} (curl (/htdocs rpath))])
                      (check-eq? status 418 reason))))]

@handbook-reference[]

@chunk[|<security:*>|
       (module+ story
         (define-tamer-suite I-am-a-teapot "Bad Users"
           #:before (check-port-ready? tamer-sakuyamon-port #:type todo)
           |<testcase: I-am-a-teapot>|))]
