#!/bin/sh

#|
# also works as solaris smf and linux systemd launcher
exec racket --require "$0"
|#

#lang at-exp racket/base

@require{../digitama/agent.rkt}
@require{../digitama/digicore.rkt}
@require{../../DigiGnome/digitama/tamer.rkt}

(require file/md5)
(require net/head)
(require net/base64)
(require web-server/http)

(require setup/dirs)
(require syntax/location)

(provide (all-defined-out))
(provide (all-from-out "../digitama/agent.rkt"))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))
(provide (all-from-out net/head net/base64 web-server/http))

(define root? (string=? (current-tamer) "root"))
(define smf-or-systemd? (getenv "SMF_METHOD"))
(define realm.rktd (path->string (build-path (digimon-stone) "realm.rktd")))

(define /htdocs (curry format "/~a"))
(define /tamer (curry format "/~~~a/~a" (current-tamer)))
(define /digimon (curry format "/~~~a:~a/~a" (current-tamer) (current-digimon)))

(define ~htdocs (curry build-path (digimon-terminus)))
(define ~tamer (curry build-path (expand-user-path (format "~~~a" (current-tamer))) "DigitalWorld" "Kuzuhamon" "terminus"))
(define ~digimon (curry build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook"))

(define tamer-errmsg (make-parameter #false))
(define tamer-port (if root? (or (sakuyamon-port) 80) 16180))
(define curl (curry sakuyamon-agent "::1" tamer-port))
(define 127.curl (curry sakuyamon-agent "127.0.0.1" tamer-port))
(define ECONNREFUSED (case (digimon-system) [{solaris} 146] [{macosx} 61] [{linux} 111]))

(define {check-ready? tips}
  (define {wrap-raise efne}
    (define errno (car (exn:fail:network:errno-errno efne)))
    (raise (cond [(not (and (eq? errno ECONNREFUSED) (tamer-errmsg))) efne]
                 [else (struct-copy exn:fail:network:errno efne
                                    [message #:parent exn (tamer-errmsg)])])))
  (thunk (with-handlers ([exn:fail:network:errno? wrap-raise])
           (curl "-X" "Options" (~a "/" tips)))))

(parameterize ([current-custodian (make-custodian)]
               [current-subprocess-custodian-mode (if smf-or-systemd? #false 'interrupt)])
  (plumber-add-flush! (current-plumber) (λ [this] (custodian-shutdown-all (current-custodian))))
  ;;; These code will be evaluated in a flexibility way.
  ; * compile one file
  ; * compile multi files
  ; * run standalone
  ; * run as scribble
  ;;; In all situations, it will fork and only fork once.
 
  (define {try-fork efne}
    (define {raise-unless-ready efne}
      (define errno (car (exn:fail:network:errno-errno efne)))
      (unless (eq? errno ECONNREFUSED) (raise efne)))

    (raise-unless-ready efne)
    (unless (find-executable-path "racket")
      (putenv "PATH" (format "~a:~a" (find-console-bin-dir) (getenv "PATH"))))
    (define-values {sakuyamon /dev/outin /dev/stdout /dev/errin}
      (subprocess #false #false #false
                  (format "~a/~a.rkt" (digimon-digivice) (current-digimon))
                  "realize" "-p" (number->string tamer-port)))

    (with-handlers ([exn:break? (compose1 (curry subprocess-kill sakuyamon) (const 'interrupt))])
      (unless (sync/enable-break /dev/outin (wrap-evt sakuyamon (const #false)))
        (tamer-errmsg (port->string /dev/errin))
        (exit (subprocess-status sakuyamon)))))

  ;;; to make the drracket background expansion happy
  (unless (regexp-match? #px#"[Dd]r[Rr]acket$" (find-system-path 'run-file))
    (when (or smf-or-systemd? (not root?)) ;;; test the deployed one
      (with-handlers ([exn:fail:network:errno? try-fork])
        ((check-ready? (file-name-from-path (quote-source-file)))))))
  (void))
