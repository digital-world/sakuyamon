#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{digicore.rkt}
@require{../../DigiGnome/digitama/posix.rkt}

(provide (except-out (all-defined-out) define-posix define-digitama static:sysinfo))
(provide (all-from-out "../../DigiGnome/digitama/posix.rkt"))

(define-ffi-definer define-posix (ffi-lib #false #:global? #true))
(define-ffi-definer define-digitama (digimon-ffi-lib "posix" #:global? #true))

@module-prefab:cstruct/no-auto-update{posix.c}

(require (submod "." prefab:posix.c))

;;; syslog
(define-digitama rsyslog
  (_fun _severity
        [topic : _symbol]
        [message : _string]
        -> _void))

;;; system-monitor
(define static:sysinfo (&ksysinfo/bzero))
(define-digitama system_statistics
  (_fun #:save-errno 'posix
        [kinfo : ksysinfo_t* = static:sysinfo]
        -> [alterrmsg : _string]
        -> (cond [(zero? (saved-errno)) kinfo]
                 [(string? alterrmsg) (raise-foreign-error 'system_statistics (saved-errno) #:strerror (lambda [libzfs-errno] alterrmsg))]
                 [else (raise-foreign-error 'system_statistics (saved-errno))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))
  (provide (all-from-out (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi)))
  (provide (all-from-out (submod ".." prefab:posix.c typed/ffi)))

  (require (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi))
  (require (submod ".." prefab:posix.c typed/ffi))

  (require/typed/provide (submod "..")
                         [rsyslog (-> Symbol Symbol String Void)]
                         [system_statistics (-> ksysinfo_t*)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." typed/ffi))

  (require "digicore.rkt")

  (define thmain : Thread (current-thread))
  (void (time (system_statistics)))
  (define task : Thread
    (timer-thread 1.0 (lambda [times] (with-handlers ([exn? (lambda [e] (break-thread thmain))])
                                        (displayln (*ksysinfo (system_statistics)))))))
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt))
  (break-thread task))
