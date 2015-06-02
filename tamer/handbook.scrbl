#lang scribble/manual

@require{tamer.rkt}

@handbook-title[]

@margin-note{@deftech{@bold{Sakuyamon}} is the Queen of @hyperlink["http://gyoudmon.org"]{gyoudmon.org}
              as well as the @deftech{@hyperlink["https://github.com/digital-world/DigiGnome#digital-world"]{@bold{digital world}}}.}

Please do not ask me why I choose @bold{Racket}. Meanwhile I don't know more than you about
whether it is suitable for this kind of tasks since its real world applications are damn harder
to be found than to be heard of. So you get it, I mean I like it and just for fun, maybe also paid for it.

It is intentionally designed not to work with @exec{raco setup}, nor any other installing processes. Another good news is that, you
are safe to run it as @bold{root}, after making as usual it will work properly.

@italic{@bold{Have tested with MacOSX, OpenIndiana and Ubuntu.}}

@tamer-smart-summary[]
@handbook-smart-table[]

@include-section[(submod "sakuyamon.rktl" doc)]
@include-section[(submod "dispatch.rktl" doc)]
@include-section[(submod "seo.rktl" doc)]
@include-section[(submod "security.rktl" doc)]
