#lang at-exp racket

(provide (all-defined-out))

@require{posix.rkt}

(define raise-foxpipe-error
  (lambda [src session]
    (raise-foreign-error src (foxpipe_last_errno session)
                         #:strerror (lambda [errno] (foxpipe_last_errmsg session)))))

(define-ffi-definer define-ssh (ffi-lib "libssh2" #:global? #true))
(define-ffi-definer define-foxpipe (digimon-ffi-lib  "foxpipe" #:global? #true))

(define-cpointer-type _foxpipe-session*)

(define-ssh libssh2_init
  (_fun #:in-original-place? #true
        [flags : _int = 0] ; 0 means init with crypto.
        -> _int))

(define-ssh libssh2_exit
  (_fun #:in-original-place? #true
        -> _void))

(define _hashtype (c-extern/enum (list 'HOSTKEY_HASH_MD5 'HOSTKEY_HASH_SHA1)))
(define _disconnect_reason (c-extern/enum (list 'DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT
                                                'DISCONNECT_PROTOCOL_ERROR
                                                'DISCONNECT_KEY_EXCHANGE_FAILED
                                                'DISCONNECT_RESERVED
                                                'DISCONNECT_MAC_ERROR
                                                'DISCONNECT_COMPRESSION_ERROR
                                                'DISCONNECT_SERVICE_NOT_AVAILABLE
                                                'DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED
                                                'DISCONNECT_HOST_KEY_NOT_VERIFIABLE
                                                'DISCONNECT_CONNECTION_LOST
                                                'DISCONNECT_BY_APPLICATION
                                                'DISCONNECT_TOO_MANY_CONNECTIONS
                                                'DISCONNECT_AUTH_CANCELLED_BY_USER
                                                'DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE
                                                'DISCONNECT_ILLEGAL_USER_NAME)))

(define-foxpipe foxpipe_last_errno
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        -> _int))

(define-foxpipe foxpipe_last_errmsg
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        [errmsg : (_ptr o _bytes)]
        [size : (_ptr o _int)]
        -> _int
        -> errmsg)
  #:c-id foxpipe_last_error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that, the allocator and deallocator will be called in atomic mode which  ;;;
;;; should be respect to the main place.                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-foxpipe foxpipe_collapse
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        [reason : _disconnect_reason = 'disconnect_by_application]
        [description : _string]
        -> [$? : _int]
        -> (cond [(zero? $?) (void)] ;;; deallocator always return void
                 [else (raise-foxpipe-error 'foxpipe_collapse session)]))
  #:wrap (deallocator))

(define-foxpipe foxpipe_construct
  (_fun #:in-original-place? #true
        [sshd_host : _string]
        [sshd_port : _short]
        [timeout_ms : _long]
        [errno : (_ptr o _int)]
        [errcode : (_ptr o _int)]
        -> [session : _foxpipe-session*/null]
        -> (or session (cond [(positive? errno) (raise-foreign-error 'foxpipe_construct errno)]
                             [else (raise-foreign-error 'foxpipe_construct errcode #:strerror gai_strerror)])))
  #:wrap (allocator foxpipe_collapse))

(define-foxpipe foxpipe_handshake
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        [type : _hashtype]
        -> [figureprint : _bytes]
        -> (or figureprint (raise-foxpipe-error 'foxpipe_handshake session))))

(define-foxpipe foxpipe_authenticate
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        [username : _string]
        [publickey : _file]
        [privatekey : _file]
        [passphrase : _string]
        -> [$? : _int]
        -> (cond [(zero? $?) $?]
                 [else (raise-foxpipe-error 'foxpipe_authenticate session)])))

(define-foxpipe foxpipe_direct_channel
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        [host-seen-by-sshd : _string]
        [service-seen-by-sshd : _uint]
        [/dev/sshin : (_ptr o _racket)]
        [/dev/sshout : (_ptr o _racket)]
        -> [$? : _int]
        -> (cond [(negative? $?) (raise-foxpipe-error 'foxpipe_direct_channel session)]
                 [else (values /dev/sshin /dev/sshout)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))
  
  (require (submod "posix.rkt" typed/ffi))
  
  (require/typed/provide (submod "..")
                         [libssh2_init (-> Integer)]
                         [libssh2_exit (-> Void)])

  (require/typed/provide/pointers Foxpipe-Session*)
  (require/typed/provide/enums hashtype disconnect_reason)

  (require/typed/provide (submod "..")
                         [foxpipe_last_errno (-> Foxpipe-Session* Integer)]
                         [foxpipe_last_errmsg (-> Foxpipe-Session* String)]
                         [foxpipe_construct (-> String Index Natural Foxpipe-Session*/Null)]
                         [foxpipe_handshake (-> Foxpipe-Session* Symbol Bytes)]
                         [foxpipe_authenticate (-> Foxpipe-Session* String Path-String Path-String String Integer)]
                         [foxpipe_collapse (-> Foxpipe-Session* String Void)]
                         [foxpipe_direct_channel (-> Foxpipe-Session* String Index (Values Input-Port Output-Port))]))
