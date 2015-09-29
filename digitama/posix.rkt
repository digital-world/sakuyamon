#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../../DigiGnome/digitama/posix.rkt}

(provide (except-out (all-defined-out) define-posix define-digitama))
(provide (all-from-out "../../DigiGnome/digitama/posix.rkt"))

(define-ffi-definer define-posix (ffi-lib #false #:global? #true))
(define-ffi-definer define-digitama (digimon-ffi-lib "posix" #:global? #true))

;;; syslog
(define-digitama rsyslog
  (_fun _severity
        [topic : _symbol]
        [message : _string]
        -> _void))

;;; system-monitor
(define-digitama vector_get_performance_stats
  (_fun [ncores : (_ptr o _long)]
        [lavg1 : (_ptr o _double)]
        [lavg5 : (_ptr o _double)]
        [lavg15 : (_ptr o _double)]
        [uptime : (_ptr o _long)]
        -> [$? : _int]
        -> (cond [(zero? $?) (vector ncores lavg1 lavg5 lavg15 uptime)]
                 [else (raise-foreign-error 'vector_get_performance_stats $?)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))
  (provide (all-from-out (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi)))

  (require "digicore.rkt")
  (require (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi))
  
  (require/typed/provide (submod "..")
                         [rsyslog (-> Symbol Symbol String Void)]
                         [vector_get_performance_stats (-> System-Status)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." typed/ffi))

  (displayln "system status:")
  (with-handlers ([exn:break? void])
    (for ([i (in-naturals 1)])
      (printf "~a: " i)
      (for ([sample (in-vector (vector_get_performance_stats))])
        (display (~a (cond [(flonum? sample) (~r sample #:precision '(= 2))]
                           [else sample])
                     #\space)))
      (newline)
      (sleep 5))))
