#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../../DigiGnome/digitama/posix.rkt}

(provide (except-out (all-defined-out) define-posix define-digitama))
(provide (all-from-out "../../DigiGnome/digitama/posix.rkt"))

(define-ffi-definer define-posix (ffi-lib "posix" #:global? #true))
(define-ffi-definer define-digitama (digimon-ffi-lib "posix" #:global? #true))

;;; syslog
(define-digitama rsyslog
  (_fun _severity
        [topic : _symbol]
        [message : _string]
        -> _void))

;;; system monitor
(define sysloadavg (c-extern 'sysloadavg (_array _double 3)))
(define-posix getloadavg
  (_fun #:save-errno 'posix
        [(_array _double 3) = sysloadavg]
        [_size = (array-length sysloadavg)]
        -> [$? : _int]
        -> (cond [($? . >= . 0) sysloadavg]
                 [else (raise-foreign-error 'getloadavg (saved-errno))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))
  (provide (all-from-out (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi)))

  (require (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi))
  
  (require/typed/provide (submod "..")
                         [sysloadavg Array]
                         [rsyslog (-> Symbol Symbol String Void)]
                         [getloadavg (-> Array)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." typed/ffi))

  (displayln "load average:")
  (with-handlers ([exn:break? void])
    (for ([i (in-naturals 1)])
      (printf "~a: " i)
      (for ([sample (in-array (getloadavg))])
        (display (~a (~r (cast sample Flonum) #:precision '(= 2)) #\space)))
      (newline)
      (sleep 5))))
