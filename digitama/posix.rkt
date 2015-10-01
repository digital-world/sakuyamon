#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{digicore.rkt}
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
; cstruct is c pointer which cannot be used as racket prefab structure.
(struct sysinfo (timestamp uptime ncores loadavg/01min loadavg/05min loadavg/15min ram/total ram/free swap/total swap/free)
  #:prefab)
                         
(define-digitama system_statistics
  (_fun [timestamp : (_ptr o _long)]
        [uptime : (_ptr o _long)]
        [ncores : (_ptr o _int)]
        [lavg1 : (_ptr o _double)]
        [lavg5 : (_ptr o _double)]
        [lavg15 : (_ptr o _double)]
        [ramtotal : (_ptr o _long)]
        [ramfree : (_ptr o _long)]
        [swaptotal : (_ptr o _long)]
        [swapfree : (_ptr o _long)]
        -> [$? : _int]
        -> (cond [(zero? $?) (sysinfo timestamp uptime ncores lavg1 lavg5 lavg15 ramtotal ramfree swaptotal swapfree)]
                 [else (raise-foreign-error 'system_statistics $?)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))
  (provide (all-from-out (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi)))

  (require (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi))

  (define-type System-Status sysinfo)
  
  (require/typed/provide (submod "..")
                         [#:struct sysinfo
                          ([timestamp : Positive-Integer]
                           [uptime : Positive-Integer]
                           [ncores : Positive-Integer]
                           [loadavg/01min : Real]
                           [loadavg/05min : Real]
                           [loadavg/15min : Real]
                           [ram/total : Positive-Integer]
                           [ram/free : Positive-Integer]
                           [swap/total : Natural]
                           [swap/free : Natural])]
                         [rsyslog (-> Symbol Symbol String Void)]
                         [system_statistics (-> sysinfo)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." typed/ffi))

  (displayln "system status:")
  (with-handlers ([exn:break? void])
    (for ([i (in-naturals 1)])
      (printf "~a: ~a~n" i (system_statistics))
      (sleep 5))))
