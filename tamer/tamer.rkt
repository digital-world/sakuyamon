#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../../DigiGnome/digitama/tamer.rkt}

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))

(current-digimon "sakuyamon")
