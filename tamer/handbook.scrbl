#lang scribble/manual

@require{tamer.rkt}

@handbook-title[]

@margin-note{@deftech{@bold{Sakuyamon}} is the Queen of @hyperlink["http://gyoudmon.org"]{gyoudmon.org}
              as well as the @deftech{@hyperlink["https://github.com/digital-world/DigiGnome#digital-world"]{@bold{digital world}}}.}

Please do not ask me why I choose @bold{Racket}. Meanwhile I don't know more than you about
whether it is suitable for this kind of tasks since its real world applications are damn harder
to be found than to be heard of. So you get it, I mean I like it and just for fun, maybe also paid for it.

The infrasturctures are designed as lightweight as possible. Instances include almost all batteries with zero configuration,
and they should communicate with each other easily. Keep it simple but also ready for large-scale extension.

It is intentionally designed not to work with @exec{raco setup}, nor any other installing process.
Please choose a non-root user and clone the source code, after making it will suitable to run as that user.

@tamer-smart-summary[]
@handbook-smart-table[]

@include-section[(submod "sakuyamon.rktl" doc)]
@include-section[(submod "dispatch.rktl" doc)]
@include-section[(submod "seo.rktl" doc)]
@include-section[(submod "security.rktl" doc)]
