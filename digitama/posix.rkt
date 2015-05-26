#lang at-exp racket

(provide (all-defined-out) saved-errno)

@require{digicore.rkt}

(require ffi/unsafe)
(require ffi/unsafe/define)
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(require (for-syntax racket/syntax))

(define-syntax {define-enum stx}
  (syntax-case stx []
    [{_ id _value} (with-syntax ([_id (format-id #'id "_~a" (syntax-e #'id))]
                                 [id.c (format-id #'id "~a.c" (syntax-e #'id))]
                                 [id.rkt (format-id #'id "~a.rkt" (syntax-e #'id))])
                     #'{begin (define _id _value)
                              (define id.c (ctype-scheme->c _id))
                              (define id.rkt (ctype-c->scheme _id))})]))

(define-ffi-definer define-posix (ffi-lib #false))
(define-ffi-definer define-digitama (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                                                         "native" (system-library-subpath #false) "posix")))

(define-posix strerror_r (_fun _int _pointer _size -> _int))

(define strerror
  {lambda [erno]
    (define errbuf (malloc 'atomic 32))
    (strerror_r erno errbuf 32)
    (bytes->string/utf-8 (car (regexp-match #px"^[^\u0]*" (make-sized-byte-string errbuf 32))))})

;;; Users and Groups

(define-posix getuid (_fun -> _uint32))
(define-posix getgid (_fun -> _uint32))
(define-posix geteuid (_fun -> _uint32))
(define-posix getegid (_fun -> _uint32))
(define-posix getppid (_fun -> _int32))
(define-posix setuid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix setgid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix seteuid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix setegid (_fun #:save-errno 'posix _uint32 -> _int))

(define-digitama fetch_tamer_ids (_fun #:save-errno 'posix
                                       _bytes {u : (_ptr o _uint32)} {g : (_ptr o _uint32)}
                                       -> {e : _int} -> (values e u g)))

(define-digitama fetch_tamer_name (_fun #:save-errno 'posix
                                         _uint32 {un : (_ptr o _bytes)}
                                         -> {e : _int} -> (values e un)))

(define-digitama fetch_tamer_group (_fun #:save-errno 'posix
                                         _uint32 {gn : (_ptr o _bytes)}
                                         -> {e : _int} -> (values e gn)))

;;; syslog.
(define-enum severity (_enum (list 'emerg    #| system is unusable |#
                                   'alert    #| action must be taken immediately |#
                                   'fatal    #| critical conditions |#
                                   'error    #| error conditions |#
                                   'warning  #| warning conditions |#
                                   'notice   #| normal but significant condition |#
                                   'info     #| informational |#
                                   'debug    #| debug-level messages |#)))

(define-digitama rsyslog (_fun _int _symbol _string -> _void))

(define syslog
  {lambda [severity topic maybe . argl]
    (rsyslog (severity.c severity) topic (apply format maybe argl))})
