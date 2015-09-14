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
(define _facility
  (_enum (list 'kernel        '= (arithmetic-shift 00 3) #| kernel messages |#
               'user          '= (arithmetic-shift 01 3) #| random user-level messages |#
               'mail          '= (arithmetic-shift 02 3) #| mail system |#
               'daemon        '= (arithmetic-shift 03 3) #| system daemons |#
               'auth          '= (arithmetic-shift 04 3) #| security/authorization messages |#
               'syslog        '= (arithmetic-shift 05 3) #| messages generated internally by syslogd |#
               'lp	           '= (arithmetic-shift 06 3) #| line printer subsystem |#
               'news          '= (arithmetic-shift 07 3) #| netnews subsystem |#
               'uucp          '= (arithmetic-shift 08 3) #| uucp subsystem |#
               'altcron       '= (arithmetic-shift 09 3) #| BSD cron/at subsystem |#
               'authpriv      '= (arithmetic-shift 10 3) #| BSD security/authorization messages |#
               'ftp	   '= (arithmetic-shift 11 3) #| file transfer subsystem |#
               'ntp	   '= (arithmetic-shift 12 3) #| network time subsystem |#
               'audit         '= (arithmetic-shift 13 3) #| audit subsystem |#
               'console       '= (arithmetic-shift 14 3) #| BSD console messages |#
               'cron          '= (arithmetic-shift 15 3) #| cron/at subsystem |#
               'local0        '= (arithmetic-shift 16 3) #| reserved for local use |#
               'local1        '= (arithmetic-shift 17 3) #| reserved for local use |#
               'local2        '= (arithmetic-shift 18 3) #| reserved for local use |#
               'local3        '= (arithmetic-shift 19 3) #| reserved for local use |#
               'local4        '= (arithmetic-shift 20 3) #| reserved for local use |#
               'local5        '= (arithmetic-shift 21 3) #| reserved for local use |#
               'local6        '= (arithmetic-shift 22 3) #| reserved for local use |#
               'local7        '= (arithmetic-shift 23 3) #| reserved for local use |#)))

(define _severity
  (_enum (list 'emerg    '= 0 #| system is unusable |#
               'alert    '= 1 #| action must be taken immediately |#
               'fatal    '= 2 #| critical conditions |#
               'error    '= 3 #| error conditions |#
               'warning  '= 4 #| warning conditions |#
               'notice   '= 5 #| normal but significant condition |#
               'info     '= 6 #| informational |#
               'debug    '= 7 #| debug-level messages |#)))

(define-digitama rsyslog
  (_fun _severity
        [topic : _symbol]
        [message : _string]
        -> _void))

(module* typed/ffi typed/racket
  ;;; Meanwhile Typed Racket does not support _pointer well
  (require/typed/provide (submod "..")
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
