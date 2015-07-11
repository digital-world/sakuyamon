#lang at-exp racket

(provide (all-defined-out))

(require file/sha1)

@require{posix.rkt}

(define-ffi-definer define-racket (ffi-lib #false))

(define-racket scheme_get_port_socket
  (_fun [/dev/tcpio : _racket]
        [fd : (_ptr o _int)]
        -> [status : _bool]
        -> (if status fd 0)))

(define-ffi-definer define-ssh (ffi-lib "libssh2" #:global? #true))

(define _libssh2_session* (_cpointer/null 'LIBSSH2_SESSION))
(define _libssh2_channel* (_cpointer/null 'LIBSSH2_CHANNEL))

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
                      (cond [(last-ssh-session) (let-values ([{_ errmsg} (libssh2_session_last_error)]) errmsg)]
                            [else "Unregistered error"]))))

(define-ssh libssh2_session_last_errno
  (_fun [session : _libssh2_session* = (last-ssh-session)]
        -> _int))

(define-ssh libssh2_session_last_error
  (_fun [session : _libssh2_session* = (last-ssh-session)]
        [errmsg : (_ptr o _bytes)]
        [size : (_ptr o _int)]
        [want-buf? : _bool = #false]
        -> [errno : _int]
        -> (values errno errmsg)))

(define-ssh libssh2_init
  ;;; this function is invoked by libssh2_session_init if needed.
  (_fun [flags : _int = 0] ; 0 means init with crypto.
        -> _int))

(define-ssh libssh2_exit
  (_fun -> _void))

(define-ssh libssh2_session_init
  (_fun [malloc : _fpointer = #false]
        [free : _fpointer = #false]
        [realoc : _fpointer = #false]
        [seed : _pointer = #false]
        -> [session : _libssh2_session*]
        -> (and (last-ssh-session session)
                (or session (raise-ssh-error 'libssh2_session_init -6))))
  #:c-id libssh2_session_init_ex)

(define-ssh libssh2_session_handshake
  (_fun [session : _libssh2_session*]
        [sshd-sockfd : _int]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'libssh2_session_handshake status)])))

(define-ssh libssh2_hostkey_hash
  (_fun [session : _libssh2_session*]
        [htype : _hashtype]
        -> [hvalue : _bytes]))

(define-ssh libssh2_userauth_list
  (_fun [session : _libssh2_session*]
        [username : _string]
        [ulength : _uint = (string-length username)]
        -> [authlist : _string]
        -> (map string->symbol (string-split authlist ","))))

(define-ssh libssh2_userauth_password
  (_fun [session : _libssh2_session*]
        [username : _string]
        [ulength : _uint = (string-length username)]
        [password : _string]
        [plength : _uint = (string-length password)]
        [pwdchange : _fpointer = #false]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'libssh2_userauth_password status)]))
  #:c-id libssh2_userauth_password_ex)

(define-ssh libssh2_userauth_publickey_fromfile
  (_fun [session : _libssh2_session*]
        [username : _string]
        [ulength : _uint = (string-length username)]
        [publickey : _file]
        [privatekey : _file]
        [passphrase : _string]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'libssh2_userauth_publickey_fromfile status)]))
  #:c-id libssh2_userauth_publickey_fromfile_ex)

(define-ssh libssh2_session_set_blocking
  (_fun [session : _libssh2_session*]
        [blocking? : _bool]
        -> _void))

(define-ssh libssh2_session_disconnect
  (_fun [session : _libssh2_session*]
        [reason : _disconnect_reason = 'SSH_DISCONNECT_BY_APPLICATION]
        [description : _string]
        [desc_language : _string = ""]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'libssh2_session_disconnect status)]))
  #:c-id libssh2_session_disconnect_ex)

(define-ssh libssh2_session_free
  (_fun [session : _libssh2_session*]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'libssh2_session_free status)])))

(define-ssh libssh2_channel_direct_tcpip
  (_fun [session : _libssh2_session*]
        [host-seen-by-sshd : _string]
        [port-seen-by-sshd : _uint]
        [sshd_host_as_localhost : _string = host-seen-by-sshd]
        [sshd_port_as_localport : _uint = 22]
        -> [channel : _libssh2_channel*]
        -> (or channel (raise-ssh-error 'libssh2_channel_direct_tcpip -6)))
  #:c-id libssh2_channel_direct_tcpip_ex)

(define-ssh libssh2_channel_write
  (_fun [channel : _libssh2_channel*]
        [SSH_EXTENDED_DATA_STDERR? : _bool = #false]
        [buffer : _bytes]
        [size : _size = (bytes-length buffer)]
        -> [status : _ssize]
        -> (cond [(exact-nonnegative-integer? status) status]
                 [else (raise-ssh-error 'libssh2_channel_write status)]))
  #:c-id libssh2_channel_write_ex)

(define-ssh libssh2_channel_write_stderr
  (_fun [channel : _libssh2_channel*]
        [SSH_EXTENDED_DATA_STDERR? : _bool = #true]
        [buffer : _bytes]
        [size : _size = (bytes-length buffer)]
        -> [status : _ssize]
        -> (cond [(exact-nonnegative-integer? status) status]
                 [else (raise-ssh-error 'libssh2_channel_write_stderr status)]))
  #:c-id libssh2_channel_write_ex)

(define-ssh libssh2_channel_read
  (_fun [channel : _libssh2_channel*]
        [SSH_EXTENDED_DATA_STDERR? : _bool = #false]
        [buffer : _bytes] ; both for input and input/output
        [size : _size = (bytes-length buffer)]
        -> [status : _ssize]
        -> (cond [(exact-nonnegative-integer? status) status]
                 [else (raise-ssh-error 'libssh2_channel_read status)]))
  #:c-id libssh2_channel_read_ex)

(define-ssh libssh2_channel_read_stderr
  (_fun [channel : _libssh2_channel*]
        [SSH_EXTENDED_DATA_STDERR? : _bool = #true]
        [buffer : _bytes] ; both for input and input/output
        [size : _size = (bytes-length buffer)]
        -> [status : _ssize]
        -> (cond [(exact-nonnegative-integer? status) status]
                 [else (raise-ssh-error 'libssh2_channel_read_stderr status)]))
  #:c-id libssh2_channel_read_ex)

(define-ssh libssh2_channel_eof?
  (_fun [channel : _libssh2_channel*]
        -> [status : _int]
        -> (cond [(negative? status) (raise-ssh-error 'libssh2_channel_read_stderr status)]
                 [else (not (zero? status))]))
  #:c-id libssh2_channel_eof)

(define-ssh libssh2_channel_free
  (_fun [channel : _libssh2_channel*]
        -> [status : _int]
        -> (cond [(zero? status) status]
                 [else (raise-ssh-error 'libssh2_session_disconnect status)])))

(define-ffi-definer define-tunnel (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                                                         "native" (system-library-subpath #false) "tunnel")))

(define-tunnel open_input_output_direct_channel
  (_fun [session : _libssh2_session*]
        [host-seen-by-sshd : _string]
        [service-seen-by-sshd : _uint]
        [inport : (_ptr o _racket)]
        [outport : (_ptr o _racket)]
        -> [status : _int]
        -> (cond [(negative? status) (raise-ssh-error 'open_input_output_direct_channel status)]
                 [else (values inport outport)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Meanwhile Typed Racket does not support CPonter well
;;; So leave the typed C to FFI itself.

(define sakuyamon-foxpipe
  (lambda [izunac host-seen-by-sshd service-seen-by-sshd /dev/stdssh
           #:username [username (current-tamer)] #:password [password #false]
           #:id_rsa.pub [rsa.pub (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")]
           #:id_rsa [id_rsa (build-path (find-system-path 'home-dir) ".ssh" "id_rsa")]
           #:passphrase [passphrase ""]]
    (parameterize ([current-custodian (make-custodian)])
      (define ssh-session (make-parameter #false))
      (define terminate/sendback-if-failed
        (lambda [maybe-exn]
          (when (exn:fail? maybe-exn) (thread-send izunac maybe-exn))
          (custodian-shutdown-all (current-custodian)) ;;; channel is managed by custodian
          (with-handlers ([exn? (const 'unsafe-but-nonsense)])
            (when (ssh-session)
              (define reason (if (exn? maybe-exn) (exn-message maybe-exn) (~a maybe-exn)))
              (libssh2_session_disconnect (ssh-session) ;;; libssh2 treats long reason as an error
                                          (substring reason 0 (min (string-length reason) 256)))
              (libssh2_session_free (ssh-session)))
            (libssh2_exit)
            (collect-garbage))))
      (with-handlers ([exn? terminate/sendback-if-failed])
        (define session (libssh2_session_init))
        (ssh-session session)
        (define ssh-socket (scheme_get_port_socket /dev/stdssh))
        (libssh2_session_handshake session ssh-socket)
        (define figureprint (libssh2_hostkey_hash session 'LIBSSH2_HOSTKEY_HASH_SHA1))
        (define authors (libssh2_userauth_list session username))
        (thread-send izunac (cons (regexp-match* #px".." (string-upcase (bytes->hex-string figureprint)))
                                  authors))
        (sleep 0)
        (libssh2_userauth_publickey_fromfile session username rsa.pub id_rsa passphrase)
        (libssh2_session_set_blocking session #false)
        (let-values ([{/dev/sshdin /dev/sshdout} (open_input_output_direct_channel session host-seen-by-sshd service-seen-by-sshd)])
          (let poll-manually ()
            ;;; libssh2 hides the socket descriptor since all channels in a session are sharing the same.
            ;;; So there is no way to wake up Racket, we have to check it on an ugly way. 
            (match (sync/timeout/enable-break 0.26149 #|Number Thoery: Meissel-Mertens Constant|# /dev/sshdin)
              [#false (poll-manually)]
              [else (let ([r (read-line /dev/sshdin)])
                      (thread-send izunac (box r))
                      (sleep 0)
                      (unless (eof-object? r)
                        (poll-manually)))]))))
      (terminate/sendback-if-failed "Remote server disconnected!"))))
