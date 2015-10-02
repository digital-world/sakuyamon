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
(struct sysinfo (timestamp uptime
                           ncores loadavg/01min loadavg/05min loadavg/15min
                           ram/total ram/free swap/total swap/free
                           bytes/recv bytes/recv/kbps bytes/send bytes/send/kbps)
  #:prefab)
                         
(define-digitama system_statistics
  (_fun [timestamp : (_ptr o _long)]
        [uptime : (_ptr o _long)]
        [ncores : (_ptr o _int)]
        [lavg1 : (_ptr o _double)]
        [lavg5 : (_ptr o _double)]
        [lavg15 : (_ptr o _double)]
        [ramtotal : (_ptr o _size)]
        [ramfree : (_ptr o _size)]
        [swaptotal : (_ptr o _size)]
        [swapfree : (_ptr o _size)]
        [bytes/recv : (_ptr o _uintmax)]
        [bytes/recv/kbps : (_ptr o _double)]
        [bytes/send : (_ptr o _uintmax)]
        [bytes/send/kbps : (_ptr o _double)]
        -> [$? : _int]
        -> (cond [(zero? $?) (sysinfo timestamp uptime ncores lavg1 lavg5 lavg15 ramtotal ramfree swaptotal swapfree
                                      bytes/recv bytes/recv/kbps bytes/send bytes/send/kbps)]
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
                           [swap/free : Natural]
                           [bytes/recv : Natural]
                           [bytes/recv/kbps : Real]
                           [bytes/send : Natural]
                           [bytes/send/kbps : Real])]
                         [rsyslog (-> Symbol Symbol String Void)]
                         [system_statistics (-> sysinfo)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." typed/ffi))

  (with-handlers ([exn:break? void])
    (for ([i (in-naturals 1)])
      (printf "~a: ~a~n" i (system_statistics))
      (sleep 1))))
