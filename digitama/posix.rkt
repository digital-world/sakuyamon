#lang at-exp racket

(provide (all-defined-out) ctype-basetype ctype-c->scheme ctype-scheme->c)
(provide (all-from-out "digicore.rkt"))
(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))
(provide (all-from-out ffi/unsafe/alloc))

@require{digicore.rkt}

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)
(require (only-in '#%foreign ctype-basetype ctype-c->scheme ctype-scheme->c))

(struct exn:foreign exn:fail (errno))

(define c-extern
  (lambda [variable ctype #:lib [lib #false]]
    (get-ffi-obj variable lib ctype)))

(define c-extern/enum
  ;;; racket->c can map multi names to one value, while c->racket uses the last name
  ;;; names in aliases will not be the value of c->racket
  (lambda [symbols [basetype _ufixint] #:map-symbol [symmap (compose1 string->symbol string-downcase symbol->string)] #:lib [lib #false]]
    (_enum (foldl (lambda [c Es] (list* (symmap c) '= (get-ffi-obj c lib basetype) Es)) null symbols) basetype)))

(define c-extern/bitmask
  (lambda [symbols [basetype _uint] #:map-symbol [symmap (compose1 string->symbol string-downcase symbol->string)] #:lib [lib #false]]
    (_bitmask (foldl (lambda [c Bs] (list* (symmap c) '= (get-ffi-obj c lib basetype) Bs)) null symbols) basetype)))

(define raise-foreign-error
  (lambda [src errno #:strerror [error->string strerror]]
    (raise (exn:foreign (format "~a: ~a; errno = ~a." src (error->string errno) errno)
                        (current-continuation-marks)
                        errno))))

(define-ffi-definer define-posix (ffi-lib #false))

(define-posix strerror
  (_fun [errno : _int]
        [buffer : (_bytes o 32)]
        [size : _size = 32]
        -> _int
        -> (bytes->string/utf-8 (car (regexp-match #px"^[^\u0]*" buffer))))
  #:c-id strerror_r)

;;; Users and Groups

(define-posix getuid (_fun -> _uint32))
(define-posix getgid (_fun -> _uint32))
(define-posix geteuid (_fun -> _uint32))
(define-posix getegid (_fun -> _uint32))
(define-posix getppid (_fun -> _int32))
(define-posix getpid (_fun -> _int32))

(define-posix setuid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'setuid (saved-errno)))))

(define-posix setgid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'setgid (saved-errno)))))

(define-posix seteuid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'seteuid (saved-errno)))))

(define-posix setegid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'setegid (saved-errno)))))

(define-ffi-definer define-digitama
  (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                       "native" (system-library-subpath #false) "posix")))

(define-digitama fetch_tamer_ids
  (_fun #:save-errno 'posix
        [username : _bytes]
        [uid : (_ptr o _uint32)]
        [gid : (_ptr o _uint32)]
        -> [$? : _int]
        -> (cond [(zero? $?) (values uid gid)]
                 [else (raise-foreign-error 'fetch_tamer_ids $?)])))

(define-digitama fetch_tamer_name
  (_fun #:save-errno 'posix
        [uid : _uint32]
        [username : (_ptr o _bytes)]
        -> [$? : _int]
        -> (cond [(zero? $?) username]
                 [else (raise-foreign-error 'fetch_tamer_name $?)])))

(define-digitama fetch_tamer_group
  (_fun #:save-errno 'posix
        [gid : _uint32]
        [groupname : (_ptr o _bytes)]
        -> [$? : _int]
        -> (cond [(zero? $?) groupname]
                 [else (raise-foreign-error 'fetch_group_name $?)])))

;;; syslog.
(define _severity (c-extern/enum (list 'EMERG 'ALERT 'FATAL 'ERROR 'WARNING 'NOTICE 'INFO 'DEBUG)))
(define _facility (c-extern/enum (list 'KERNEL 'USER 'MAIL 'DAEMON 'AUTH 'SYSLOG 'LPR 'NEWS
                                       'UUCP 'ALTCRON 'AUTHPRIV 'FTP 'NTP 'AUDIT 'CONSOLE 'CRON
                                       'LOCAL0 'LOCAL1 'LOCAL2 'LOCAL3 'LOCAL4 'LOCAL5 'LOCAL6 'LOCAL7)))

(define-digitama rsyslog
  (_fun _severity
        [topic : _symbol]
        [message : _string]
        -> _void))

(module* typed/ffi typed/racket
  ;;; Meanwhile Typed Racket does not support _pointer well
  (require/typed/provide (submod "..")
                         [#:opaque CType ctype?]
                         [#:struct (exn:foreign exn) ([errno : Integer])]
                         [strerror (-> Natural String)]
                         [getuid (-> Natural)]
                         [getgid (-> Natural)]
                         [geteuid (-> Natural)]
                         [getegid (-> Natural)]
                         [seteuid (-> Natural Void)]
                         [setegid (-> Natural Void)]
                         [fetch_tamer_ids (-> Bytes (Values Natural Natural))]
                         [fetch_tamer_name (-> Natural Bytes)]
                         [fetch_tamer_group (-> Natural Bytes)]
                         [rsyslog (-> Symbol Symbol String Void)]))
