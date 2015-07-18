#lang at-exp racket

(provide (all-defined-out))

(require file/sha1)

@require{posix.rkt}

(define-ffi-definer define-ssh (ffi-lib "libssh2" #:global? #true))

(define-cstruct _Foxpipe-Session
  ([ssh_session (_cpointer/null 'libssh2_session)]
   [dev_tcpin _racket]
   [dev_tcpout _racket]))

(define _foxpipe_session* _Foxpipe-Session-pointer/null)

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

(define last-ssh-session (make-parameter #false))

(define raise-ssh-error
  (curry raise-foreign-error
         #:strerror (lambda [errno]
                      (cond [(last-ssh-session) (let-values ([{_ errmsg} (foxpipe_last_error)]) errmsg)]
                            [else "Unregistered error"]))))

(define-ssh libssh2_init
  ;;; this function will be invoked by libssh2_session_init if needed.
  (_fun [flags : _int = 0] ; 0 means init with crypto.
        -> _int))

(define-ssh libssh2_exit
  (_fun -> _void))

(define-ffi-definer define-tunnel (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                                                         "native" (system-library-subpath #false) "foxpipe")))

(define-tunnel foxpipe_construct
  (_fun [tcp-connect : _racket = tcp-connect/enable-break]
        [sshd_host : _racket]
        [sshd_port : _racket]
        -> [session : _foxpipe_session*]))

(define-tunnel foxpipe_handshake
  (_fun [session : _foxpipe_session*]
        [type : _hashtype]
        -> [figureprint : _bytes]
        -> (or figureprint (raise-ssh-error 'foxpipe_handshake (foxpipe_last_errno)))))

(define-tunnel foxpipe_authenticate
  (_fun [session : _foxpipe_session*]
        [username : _string]
        [publickey : _file]
        [privatekey : _file]
        [passphrase : _string]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'foxpipe_authenticate status)])))

(define-tunnel foxpipe_collapse
  (_fun [session : _foxpipe_session*]
        [reason : _disconnect_reason = 'SSH_DISCONNECT_BY_APPLICATION]
        [description : _string]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'foxpipe_collapse status)])))

(define-tunnel foxpipe_last_errno
  (_fun [session : _foxpipe_session* = (last-ssh-session)]
        -> _int))

(define-tunnel foxpipe_last_error
  (_fun [session : _foxpipe_session* = (last-ssh-session)]
        [errmsg : (_ptr o _bytes)]
        [size : (_ptr o _int)]
        -> [errno : _int]
        -> (values errno errmsg)))

(define-tunnel foxpipe_direct_channel
  (_fun [session : _foxpipe_session*]
        [host-seen-by-sshd : _string]
        [service-seen-by-sshd : _uint]
        [/dev/sshin : (_ptr o _racket)]
        [/dev/sshout : (_ptr o _racket)]
        -> [status : _int]
        -> (cond [(negative? status) (raise-ssh-error 'foxpipe_direct_channel status)]
                 [else (values /dev/sshin /dev/sshout)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Meanwhile Typed Racket does not support CPonter well
;;; So leave the "Typed C" to FFI itself.

(define sakuyamon-foxpipe
  (lambda [izunac]
    (with-handlers ([exn:break? void])
      (define argh (place-channel-get izunac))
      (match-define (list timeout sshd-host host-seen-by-sshd service-seen-by-sshd)
        (map (curry hash-ref argh) '(timeout sshd-host host-seen-by-sshd service-seen-by-sshd)))
      (match-define (list username passphrase rsa.pub id_rsa)
        (map (curry hash-ref argh)
             '(username passphrase rsa.pub id_rsa)
             (list (current-tamer) ""
                   (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")
                   (build-path (find-system-path 'home-dir) ".ssh" "id_rsa"))))
      
      (let on-sighup ()
        (define channel-custodian (make-custodian))
        (define ssh-session (make-parameter #false))
        (define terminate/sendback-if-failed
          (lambda [maybe-exn]
            (when (exn:fail? maybe-exn)
              (place-channel-put izunac (list sshd-host 'fail (exn-message maybe-exn))))
            (with-handlers ([exn? displayln])
              (when (ssh-session) ;;; libssh2 treats long reason as an error
                (define reason (if (exn? maybe-exn) (exn-message maybe-exn) (~a maybe-exn)))
                (custodian-shutdown-all channel-custodian) ;;; This also releases libssh2_channel
                (foxpipe_collapse (ssh-session) (substring reason 0 (min (string-length reason) 256))))
              (collect-garbage))
            (if (exn:break:terminate? maybe-exn) (libssh2_exit) (on-sighup))))
        (with-handlers ([exn? terminate/sendback-if-failed])
          (place-channel-put izunac (list sshd-host 'notify "Connecting to sshd:~a." 22))
          (define session (foxpipe_construct sshd-host 22))
          (ssh-session session)
          (define figureprint (foxpipe_handshake session 'LIBSSH2_HOSTKEY_HASH_SHA1))
          (place-channel-put izunac (cons sshd-host (regexp-match* #px".." (string-upcase (bytes->hex-string figureprint)))))
          (foxpipe_authenticate session username rsa.pub id_rsa passphrase)
          (parameterize ([current-custodian channel-custodian])
            (define-values [/dev/sshdin /dev/sshdout] (foxpipe_direct_channel session host-seen-by-sshd service-seen-by-sshd))
            (let poll-manually ()
              (match (sync/timeout/enable-break timeout (Foxpipe-Session-dev_tcpin session))
                [(? false?)
                 (let ([reason (place-channel-put/get izunac (cons sshd-host timeout))])
                   (unless (false? reason)
                     (error 'ssh-channel "foxpipe has to collapse: ~a!" reason)))]
                [(? input-port?)
                 (let ([r (read /dev/sshdin)])
                   (place-channel-put izunac (cons sshd-host (vector r)))
                   (cond [(eof-object? r) (error 'ssh-channel "remote server disconnected!")]
                         [else (poll-manually)]))]))))))))
  