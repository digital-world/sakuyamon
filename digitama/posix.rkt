#lang at-exp racket

(provide (all-defined-out) saved-errno)

@require{digicore.rkt}

(require ffi/unsafe)
(require ffi/unsafe/define)
      
(define-ffi-definer define-posix (ffi-lib #false))
(define-ffi-definer define-digitama (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                                                         "native" (system-library-subpath #false) "posix")))

(define-posix getuid (_fun -> _uint32))
(define-posix getgid (_fun -> _uint32))
(define-posix setuid (_fun #:save-errno 'posix _uint32 -> _int))
(define-posix setgid (_fun #:save-errno 'posix _uint32 -> _int))

(define-posix strerror_r (_fun _int _pointer _size -> _int))

(define strerror
  {lambda [erno]
    (define errbuf (malloc 'atomic 32))
    (strerror_r erno errbuf 32)
    (bytes->string/utf-8 (car (regexp-match #px"^[^\u0]*" (make-sized-byte-string errbuf 32))))})



(define-digitama fetch_tamer_ids (_fun #:save-errno 'posix
                                       _bytes {u : (_ptr o _uint32)} {g : (_ptr o _uint32)}
                                       -> {e : _int} -> (values e u g)))

(define-digitama fetch_tamer_name (_fun #:save-errno 'posix
                                         _uint32 {un : (_ptr o _bytes)}
                                         -> {e : _int} -> (values e un)))

(define-digitama fetch_tamer_group (_fun #:save-errno 'posix
                                         _uint32 {gn : (_ptr o _bytes)}
                                         -> {e : _int} -> (values e gn)))