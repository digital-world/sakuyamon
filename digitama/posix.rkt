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
                           fs/total fs/free disk/rkbps disk/wkbps
                           nic/received nic/rkbps nic/sent nic/skbps)
  #:prefab)
                         
(define-digitama system_statistics
  (_fun #:save-errno 'posix
        [timestamp : (_ptr o _long)]
        [uptime : (_ptr o _long)]
        [ncores : (_ptr o _int)]
        [lavg1 : (_ptr o _double)]
        [lavg5 : (_ptr o _double)]
        [lavg15 : (_ptr o _double)]
        [ramtotal : (_ptr o _size)]
        [ramfree : (_ptr o _size)]
        [swaptotal : (_ptr o _size)]
        [swapfree : (_ptr o _size)]
        [fstotal : (_ptr o _size)]
        [fsfree : (_ptr o _uintmax)]
        [disk-rkbps : (_ptr o _double)]
        [disk-wkbps : (_ptr o _double)]
        [nic-rkb : (_ptr o _uintmax)]
        [nic-rkbps : (_ptr o _double)]
        [nic-skb : (_ptr o _uintmax)]
        [nic-skbps : (_ptr o _double)]
        -> [alterrmsg : _string]
        -> (cond [(zero? (saved-errno)) (sysinfo timestamp uptime ncores lavg1 lavg5 lavg15 ramtotal ramfree swaptotal swapfree
                                                 fstotal fsfree disk-rkbps disk-wkbps nic-rkb nic-rkbps nic-skb nic-skbps)]
                 [(string? alterrmsg) (raise-foreign-error 'system_statistics (saved-errno) #:strerror (lambda [libzfs-errno] alterrmsg))]
                 [else (raise-foreign-error 'system_statistics (saved-errno))])))

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
                           [fs/total : Natural]
                           [fs/free : Natural]
                           [disk/rkbps : Real]
                           [disk/wkbps : Real]
                           [nic/received : Natural]
                           [nic/rkbps : Real]
                           [nic/sent : Natural]
                           [nic/skbps : Real])]
                         [rsyslog (-> Symbol Symbol String Void)]
                         [system_statistics (-> sysinfo)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." typed/ffi))

  (require "digicore.rkt")

  (define thmain : Thread (current-thread))
  (void (time (system_statistics)))
  (define boottime : Real (current-inexact-milliseconds))
  (define task : Thread
    (timer-thread 1.0 (lambda [times] (with-handlers ([exn? (lambda [e] (break-thread thmain))])
                                        (printf "~a| ~a~n"
                                                (~r (/ (- (current-inexact-milliseconds) boottime) 1000.0)
                                                    #:precision '(= 6)
                                                    #:min-width 12)
                                                (system_statistics))))))
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt))
  (break-thread task))
