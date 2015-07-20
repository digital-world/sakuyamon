#lang at-exp racket

(provide (all-defined-out))

@require{posix.rkt}

(define raise-foxpipe-error
  (lambda [src session]
    (raise-foreign-error src (foxpipe_last_errno session)
                         #:strerror (lambda [errno] (foxpipe_last_errmsg session)))))

(define-ffi-definer define-ssh (ffi-lib "libssh2" #:global? #true))

(define _foxpipe_session* (_cpointer/null 'foxpipe_session))

(define _hashtype
  (_enum (list 'LIBSSH2_HOSTKEY_HASH_MD5  '= 1
               'LIBSSH2_HOSTKEY_HASH_SHA1 '= 2)))

(define _disconnect_reason
  (_enum (list 'SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT    '= 1
               'SSH_DISCONNECT_PROTOCOL_ERROR                 '= 2
               'SSH_DISCONNECT_KEY_EXCHANGE_FAILED            '= 3
               'SSH_DISCONNECT_RESERVED                       '= 4
               'SSH_DISCONNECT_MAC_ERROR                      '= 5
               'SSH_DISCONNECT_COMPRESSION_ERROR              '= 6
               'SSH_DISCONNECT_SERVICE_NOT_AVAILABLE          '= 7
               'SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED '= 8
               'SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE        '= 9
               'SSH_DISCONNECT_CONNECTION_LOST                '= 10
               'SSH_DISCONNECT_BY_APPLICATION                 '= 11
               'SSH_DISCONNECT_TOO_MANY_CONNECTIONS           '= 12
               'SSH_DISCONNECT_AUTH_CANCELLED_BY_USER         '= 13
               'SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE '= 14
               'SSH_DISCONNECT_ILLEGAL_USER_NAME              '= 15)))

(define-ssh libssh2_init
  ;;; this function will be invoked by libssh2_session_init if needed.
  (_fun [flags : _int = 0] ; 0 means init with crypto.
        -> _int))

(define-ssh libssh2_exit
  (_fun -> _void))

(define-ffi-definer define-tunnel
  (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                       "native" (system-library-subpath #false) "foxpipe")))

(define-tunnel foxpipe_last_errno
  (_fun [session : _foxpipe_session*]
        -> _int))

(define-tunnel foxpipe_last_errmsg
  (_fun [session : _foxpipe_session*]
        [errmsg : (_ptr o _bytes)]
        [size : (_ptr o _int)]
        -> [errno : _int]
        -> errmsg)
  #:c-id foxpipe_last_error)

(define-tunnel foxpipe_construct
  (_fun [tcp-connect : _racket = tcp-connect/enable-break]
        [sshd_host : _racket]
        [sshd_port : _racket]
        -> [session : _foxpipe_session*]))

(define-tunnel foxpipe_handshake
  (_fun [session : _foxpipe_session*]
        [type : _hashtype]
        -> [figureprint : _bytes]
        -> (or figureprint (raise-foxpipe-error 'foxpipe_handshake session))))

(define-tunnel foxpipe_authenticate
  (_fun [session : _foxpipe_session*]
        [username : _string]
        [publickey : _file]
        [privatekey : _file]
        [passphrase : _string]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-foxpipe-error 'foxpipe_authenticate session)])))

(define-tunnel foxpipe_collapse
  (_fun [session : _foxpipe_session*]
        [reason : _disconnect_reason = 'SSH_DISCONNECT_BY_APPLICATION]
        [description : _string]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-foxpipe-error 'foxpipe_collapse session)])))

(define-tunnel foxpipe_direct_channel
  (_fun [session : _foxpipe_session*]
        [host-seen-by-sshd : _string]
        [service-seen-by-sshd : _uint]
        [/dev/sshin : (_ptr o _racket)]
        [/dev/sshout : (_ptr o _racket)]
        -> [status : _int]
        -> (cond [(negative? status) (raise-foxpipe-error 'foxpipe_direct_channel session)]
                 [else (values /dev/sshin /dev/sshout)])))
