#!/bin/sh

#|
# also works as solaris smf and linux systemd launcher
exec racket -t "$0" -- ${1+"$@"}
|#

#lang at-exp racket/base

@require{../digitama/agent.rkt}
@require{../digitama/digicore.rkt}
@require{../../wisemon/digitama/tamer.rkt}

(require file/md5)
(require net/head)
(require net/base64)
(require web-server/http)

(require setup/dirs)
(require syntax/location)

(provide (all-defined-out))
(provide (all-from-out "../digitama/agent.rkt"))
(provide (all-from-out net/head net/base64 web-server/http))
(provide (except-out (all-from-out "../digitama/digicore.rkt" "../../wisemon/digitama/tamer.rkt")
                     exn:break:hang-up? exn:break:terminate?))

(unless (find-executable-path "racket")
  (void (putenv "PATH" (format "~a:~a" (find-console-bin-dir) (getenv "PATH")))))

(define root? (string=? (current-tamer) "root"))
(define smf-or-systemd? (getenv "SMF_METHOD"))
(define-values {daemonize-sakuyamon? daemonize-foxpipe?}
  (match (current-command-line-arguments)
    [{vector "sakuyamon"} (values #true #false)]
    [{vector "foxpipe"} (values #false #true)]
    [else (values #false #false)]))

(define /htdocs (curry format "/~a"))
(define /tamer (curry format "/~~~a/~a" (current-tamer)))
(define /digimon (curry format "/~~~a:~a/~a" (current-tamer) (current-digimon)))

(define ~htdocs (curry build-path (digimon-terminus)))
(define ~tamer (curry build-path (expand-user-path (format "~~~a" (current-tamer))) "DigitalWorld" "Kuzuhamon" "terminus"))
(define ~digimon (curry build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook"))

(define realm.rktd (path->string (build-path (digimon-stone) "realm.rktd")))

(define tamer-errmsg (make-hash))
(define tamer-sakuyamon-port (if root? (or (sakuyamon-port) 80) 16180))
(define tamer-foxpipe-port (sakuyamon-foxpipe-port))
(define curl (curry sakuyamon-agent "::1" tamer-sakuyamon-port))
(define 127.curl (curry sakuyamon-agent "127.0.0.1" tamer-sakuyamon-port))

(define check-port-ready?
  (lambda [port #:type fraise]
    (thunk (with-handlers ([exn:fail:network:errno?
                            (lambda [efne]
                              (define $port (cadr (regexp-match #px"port number: (\\d+)" (exn-message efne))))
                              (define errmsg (hash-ref tamer-errmsg (string->number $port) (const #false)))
                              (define errno (car (exn:fail:network:errno-errno efne)))
                              (if (equal? fraise error)
                                  (raise (cond [(or (false? errmsg) (not (eq? errno ECONNREFUSED))) efne]
                                               [else (struct-copy exn:fail:network:errno efne
                                                                  [message #:parent exn errmsg])]))
                                  (fraise #| skip or todo|# (or errmsg (exn-message efne)))))])
             (define-values [/dev/tcpin /dev/tcpout] (tcp-connect "localhost" port))
             (tcp-abandon-port /dev/tcpin)
             (tcp-abandon-port /dev/tcpout)))))

(parameterize ([current-custodian (make-custodian)]
               [current-subprocess-custodian-mode (if smf-or-systemd? #false 'interrupt)])
  (plumber-add-flush! (current-plumber) (λ [this] (custodian-shutdown-all (current-custodian))))
  ;;; These code will be evaluated in a flexibility way.
  ; * compile one file
  ; * compile multi files
  ; * run standalone
  ; * run as scribble
  ;;; In all situations, it will fork and only fork once.

  (define (pwait child /dev/outin /dev/errin tamer-port)
    (with-handlers ([exn:break? (compose1 (curry subprocess-kill child) (const 'interrupt))])
      (hash-remove! tamer-errmsg tamer-port)
      (unless (sync/enable-break /dev/outin (wrap-evt child (const #false)))
        (hash-set! tamer-errmsg tamer-port (port->string /dev/errin))
        (exit (subprocess-status child)))))

  (define raise-unless-ready
    (lambda [efne]
      (define errno (car (exn:fail:network:errno-errno efne)))
      (unless (eq? errno ECONNREFUSED) (raise efne))))
  
  (define (try-fork-sakuyamon efne)
    (raise-unless-ready efne)
    (define-values [sakuyamon /dev/outin /dev/stdout /dev/errin]
      (subprocess #false #false #false (format "~a/~a.rkt" (digimon-digivice) (current-digimon))
                  "realize" "-p" (number->string tamer-sakuyamon-port)))
    (pwait sakuyamon /dev/outin /dev/errin tamer-sakuyamon-port))

  (define (try-fork-foxpipe efne)
    (raise-unless-ready efne)
    (define-values [foxpipe /dev/outin /dev/stdout /dev/errin]
      (subprocess #false #false #false (format "~a/~a.rkt" (digimon-digivice) (current-digimon))
                  "foxpipe"))
    (pwait foxpipe /dev/outin /dev/errin tamer-foxpipe-port))

  ;;; to make the drracket background expansion happy
  ;;; and to test the deployed ones when running as root
  (unless (regexp-match? #px#"[Dd]r[Rr]acket$" (find-system-path 'run-file))
    (when (or (and smf-or-systemd? daemonize-sakuyamon?) (not root?))
      (with-handlers ([exn:fail:network:errno? try-fork-sakuyamon])
        ((check-port-ready? tamer-sakuyamon-port #:type error))))
    (when (and smf-or-systemd? daemonize-foxpipe?)
      (with-handlers ([exn:fail:network:errno? try-fork-foxpipe])
        ((check-port-ready? tamer-foxpipe-port #:type error)))))
  (void))
