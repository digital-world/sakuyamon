#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))

@require{digicore.rkt}

(require ffi/unsafe)
(require ffi/unsafe/define)

(define-ffi-definer define-posix (ffi-lib #false))
(define-ffi-definer define-digitama (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                                                         "native" (system-library-subpath #false) "posix")))

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
(define-posix setuid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix setgid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix seteuid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix setegid (_fun #:save-errno 'posix _uint32 -> _int))

(define-digitama fetch_tamer_ids
  (_fun #:save-errno 'posix
        [username : _bytes]
        [uid : (_ptr o _uint32)]
        [gid : (_ptr o _uint32)]
        -> [errno : _int]
        -> (values errno uid gid)))

(define-digitama fetch_tamer_name
  (_fun #:save-errno 'posix
        [uid : _uint32]
        [username : (_ptr o _bytes)]
        -> [errno : _int]
        -> (values errno username)))

(define-digitama fetch_tamer_group
  (_fun #:save-errno 'posix
        [gid : _uint32]
        [groupname : (_ptr o _bytes)]
        -> [errno : _int]
        -> (values errno groupname)))

;;; syslog.
(define _severity
  (_enum (list 'emerg    #| system is unusable |#
               'alert    #| action must be taken immediately |#
               'fatal    #| critical conditions |#
               'error    #| error conditions |#
               'warning  #| warning conditions |#
               'notice   #| normal but significant condition |#
               'info     #| informational |#
               'debug    #| debug-level messages |#)))

(define-digitama rsyslog
  (_fun _severity
        [topic : _symbol]
        [message : _string]
        -> _void))
