#!/bin/sh

#|
# also works as solaris smf and linux systemd launcher
exec racket -t "$0" -- ${1+"$@"}
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

(unless (find-executable-path "racket")
  (void (putenv "PATH" (format "~a:~a" (find-console-bin-dir) (getenv "PATH")))))

(define root? (string=? (current-tamer) "root"))
(define smf-or-systemd? (getenv "SMF_METHOD"))
(define rsyslogd? (or (find-executable-path "rsyslogd")
                      (file-exists? "/usr/lib/rsyslog/rsyslogd")))
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

(define ECONNREFUSED (case (digimon-system) [{solaris} 146] [{macosx} 61] [{linux} 111]))
(define tamer-sakuyamon-errmsg (make-parameter #false))
(define tamer-sakuyamon-port (if root? (or (sakuyamon-port) 80) 16180))
(define tamer-foxpipe-errmsg (make-parameter #false))
(define tamer-foxpipe-port (if root? (sakuyamon-foxpipe-port) (kuzuhamon-foxpipe-port)))
(define curl (curry sakuyamon-agent "::1" tamer-sakuyamon-port))
(define 127.curl (curry sakuyamon-agent "127.0.0.1" tamer-sakuyamon-port))

(define {raise-unless-ready efne}
  (define errno (car (exn:fail:network:errno-errno efne)))
  (unless (eq? errno ECONNREFUSED) (raise efne)))

(define {wrap-raise errmsg efne}
  (define errno (car (exn:fail:network:errno-errno efne)))
  (raise (cond [(not (and (eq? errno ECONNREFUSED) errmsg)) efne]
               [else (struct-copy exn:fail:network:errno efne
                                  [message #:parent exn errmsg])])))

(define {check-sakuyamon-ready? tips}
  (thunk (with-handlers ([exn:fail:network:errno? (curry wrap-raise (tamer-sakuyamon-errmsg))])
           (curl "-X" "Options" (~a "/" tips)))))

(define {check-foxpipe-ready? #:close? [close? #true]}
  (thunk (with-handlers ([exn:fail:network:errno? (curry wrap-raise (tamer-foxpipe-errmsg))])
           (define ports (foxpipe-connect "localhost" tamer-foxpipe-port #:retry 0))
           (when close?
             (tcp-abandon-port (car ports))
             (tcp-abandon-port (cdr ports)))
           ports)))
 
(parameterize ([current-custodian (make-custodian)]
               [current-subprocess-custodian-mode (if smf-or-systemd? #false 'interrupt)])
  (plumber-add-flush! (current-plumber) (λ [this] (custodian-shutdown-all (current-custodian))))
  ;;; These code will be evaluated in a flexibility way.
  ; * compile one file
  ; * compile multi files
  ; * run standalone
  ; * run as scribble
  ;;; In all situations, it will fork and only fork once.

  (define {pwait child /dev/outin /dev/errin tamer-errmsg}
    (with-handlers ([exn:break? (compose1 (curry subprocess-kill child) (const 'interrupt))])
      (unless (sync/enable-break /dev/outin (wrap-evt child (const #false)))
        (tamer-errmsg (port->string /dev/errin))
        (exit (subprocess-status child)))))
  
  (define {try-fork-sakuyamon efne}
    (raise-unless-ready efne)
    (define-values {sakuyamon /dev/outin /dev/stdout /dev/errin}
      (subprocess #false #false #false
                  (format "~a/~a.rkt" (digimon-digivice) (current-digimon))
                  "realize" "-p" (number->string tamer-sakuyamon-port)))
    (pwait sakuyamon /dev/outin /dev/errin tamer-sakuyamon-errmsg))

  (define {try-fork-foxpipe efne}
    (raise-unless-ready efne)
    (define-values {foxpipe /dev/outin /dev/stdout /dev/errin}
      (subprocess #false #false #false
                  (format "~a/~a.rkt" (digimon-digivice) (current-digimon))
                  "foxpipe"))
    (pwait foxpipe /dev/outin /dev/errin tamer-foxpipe-errmsg))

  ;;; to make the drracket background expansion happy
  ;;; and to test the deployed ones when running as root
  (unless (regexp-match? #px#"[Dd]r[Rr]acket$" (find-system-path 'run-file))
    (when (or (and smf-or-systemd? daemonize-sakuyamon?) (not root?))
      (with-handlers ([exn:fail:network:errno? try-fork-sakuyamon])
        ((check-sakuyamon-ready? (file-name-from-path (quote-source-file))))))
    (when (and rsyslogd? (or (and smf-or-systemd? daemonize-foxpipe?) (not root?)))
      (with-handlers ([exn:fail:network:errno? try-fork-foxpipe])
        ((check-foxpipe-ready? #:close? #true)))))
  (void))
