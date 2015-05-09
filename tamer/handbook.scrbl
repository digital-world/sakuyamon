#lang scribble/manual

@require{tamer.rkt}

@handbook-title[]

@margin-note{@deftech{@bold{Sakuyamon}} is the Queen of @hyperlink["http://gyoudmon.org"]{gyoudmon.org}.}

Please do not ask me why I choose @hyperlink["http://www.racket-lang.org"]{@bold{Racket}} to build my website.
Meanwhile I don@literal{'}t know more than you about whether it is suitable for this kind of tasks since its
real world applications are damn harder to be found than to be heard of. So you get it, I mean I like it and just
for fun, maybe also paid for it.

Due to my poor knowledge on @bold{Racket}, infrasturctures are designed as lightweight as possible. Instances
include almost all batteries with zero configuration, and they should communicate with each other easily. Keep it
simple but also ready for large-scale extension.

@tamer-smart-summary[]
@handbook-smart-table[]

@include-section[(submod "sakuyamon.rkt" doc)]
@include-section[(submod "dispatch.rkt" doc)]
