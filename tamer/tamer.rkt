#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../../DigiGnome/digitama/tamer.rkt}
@require{../digitama/digicore.rkt}

(require web-server/test) 

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out web-server/test))
