#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

This story is designed for detecting the status of features and bugs of @bold{Racket} itself.

@chunk[|<racket taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)
       <racket:*>]

@handbook-story{Hello, Racket!}

@handbook-scenario{Typed Libraries}

@tamer-note['typed-libraries]

@chunk[|<testcase: typed-libraries>|
       (test-exn "Web Application"
                 exn:fail:filesystem:missing-module?
                 {λ _ (dynamic-require 'typed/web-server/http 'response/full)})]

@handbook-scenario{Typed Submodules}

@tamer-action[(namespace-variable-value 'digimon-zone)]

@handbook-appendix[]

@chunk[<racket:*>
       {module+ main (call-as-normal-termination tamer-prove)}
       {module+ story
         (define-tamer-suite typed-libraries "Typed Racket Libraries!"
           |<testcase: typed-libraries>|)}]
