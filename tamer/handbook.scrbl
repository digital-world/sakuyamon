#lang scribble/manual

@require{tamer.rkt}

@handbook-title[]

@tech{@#%digimon} is the Queen of @hyperlink["http://gyoudmon.org"]{gyoudmon.org}.

It is intentionally designed not to work with @exec{raco setup}, nor any other installing processes. Another good news is that, you
are safe to run it as @bold{root}, after making as usual it will work properly.

@italic{@bold{Have tested with MacOSX, OpenIndiana.}}

@tamer-smart-summary[]
@handbook-smart-table[]

@include-section[(submod "sakuyamon.rkt" doc)]
@include-section[(submod "dispatch.rkt" doc)]
@include-section[(submod "seo.rkt" doc)]
@include-section[(submod "security.rkt" doc)]

@handbook-appendix[#:index? #true]
