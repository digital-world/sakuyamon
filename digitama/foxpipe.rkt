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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that, the allocator and deallocator will be called in atomic mode which should be respect to the main place.       ;;;
;;; This also result in other APIs having to be marked with #:in-original-place? #true                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foxpipe foxpipe_collapse
  (_fun #:in-original-place? #true
        [session : _foxpipe-session*]
        [reason : _disconnect_reason = 'disconnect_by_application]
        [description : _string]
        -> [$? : _int]
        -> (cond [(zero? $?) $?]
                 [else (raise-foxpipe-error 'foxpipe_collapse session)]))
  #:wrap (deallocator)) ;;; deallocator always return void

(define-foxpipe foxpipe_construct
  (_fun #:in-original-place? #true
        [tcp-connect : _racket = tcp-connect/enable-break]
        [sshd_host : _racket]
        [sshd_port : _racket]
        -> [session : _foxpipe-session*/null])
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

  (require/typed/provide (submod "..")
                         [foxpipe_last_errno (-> Foxpipe-Session* Integer)]
                         [foxpipe_last_errmsg (-> Foxpipe-Session* String)]
                         [foxpipe_construct (-> String Integer Foxpipe-Session*/Null)]
                         [foxpipe_handshake (-> Foxpipe-Session* Symbol Bytes)]
                         [foxpipe_authenticate (-> Foxpipe-Session* String Path-String Path-String String Integer)]
                         [foxpipe_collapse (-> Foxpipe-Session* String Void)]
                         [foxpipe_direct_channel (-> Foxpipe-Session* String Index (Values Input-Port Output-Port))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* foxpipe typed/racket
  (provide (all-defined-out))
  
  (require (submod ".." typed/ffi))
  (require (submod "posix.rkt" typed/ffi))
  
  (require "digicore.rkt")
  
  (require/typed file/sha1
                 [bytes->hex-string (-> Bytes String)])

  (define the-one-manages-foxpipe-or-plaintcp-io : (Parameterof Custodian) (make-parameter (make-custodian)))
  (define ssh-session : (Parameterof Foxpipe-Session*/Null) (make-parameter #false))

  (define realize : Place-Main
    (lambda [izunac]
      (with-handlers ([exn:break? void])
        (define/extract-symtable (place-channel-get izunac)
          [sshd-host : String = "localhost"]
          [host-seen-by-sshd : String = "localhost"]
          [service-seen-by-sshd : Index = 514]
          [plaintransport? : Boolean = #false]
          [username : String = (current-tamer)]
          [passphrase : String = ""]
          [rsa.pub : Path-String = (build-path (find-system-path 'home-dir) ".ssh" "id_rsa.pub")]
          [id_rsa : Path-String = (build-path (find-system-path 'home-dir) ".ssh" "id_rsa")])

        (define (on-collapsed [signal : exn]) : Void
          (define session : Foxpipe-Session*/Null (ssh-session))
          (with-handlers ([exn? (lambda [[e : exn]] (place-channel-put izunac (list sshd-host 'error (exn-message e))))])
            (when (exn:fail? signal)
              (place-channel-put izunac (list sshd-host 'fail (exn-message signal))))
            ;;; shutdown channels and plaintcp ports
            (custodian-shutdown-all (the-one-manages-foxpipe-or-plaintcp-io))
            (when (foxpipe-session*? session)
              ;;; shutdown ssh tcp ports
              (foxpipe_collapse session (~a signal))
              (when (exn:break? signal)
                (libssh2_exit)))))
        
        (with-handlers ([exn:break? on-collapsed])
          (let catch-send-clear-loop : Void ()
            (the-one-manages-foxpipe-or-plaintcp-io (make-custodian))
            (with-handlers ([exn:fail? on-collapsed])
              (match-define-values ((list session) cputime wallclock gctime)
                ((inst time-apply Foxpipe-Session*/Null Any)
                 (thunk (collect-garbage)
                        (place-channel-put izunac (list sshd-host 'notify "constructing ssh channel"))
                        (define session : Foxpipe-Session*/Null (foxpipe_construct sshd-host 22))
                        (when (foxpipe-session*? session)
                          (ssh-session session)
                          (define figureprint : String (bytes->hex-string (foxpipe_handshake session 'hostkey_hash_sha1)))
                          (place-channel-put izunac (cons sshd-host (regexp-match* #px".." (string-upcase figureprint))))
                          (foxpipe_authenticate session username rsa.pub id_rsa passphrase))
                        (ssh-session))
                 null))
              (parameterize ([current-custodian (the-one-manages-foxpipe-or-plaintcp-io)])
                (define maxinterval : Positive-Real (+ (sakuyamon-foxpipe-idle) (/ (+ cputime gctime) 1000.0)))
                (define-values (/dev/tcpin /dev/tcpout)
                  (foxpipe_direct_channel (cast session Foxpipe-Session*) host-seen-by-sshd service-seen-by-sshd))
                (let recv-match-send-loop : Void ()
                  (match (sync/timeout/enable-break maxinterval /dev/tcpin)
                    [(? false?) (let ([reason (place-channel-put/get izunac (cons sshd-host maxinterval))])
                                  (unless (false? reason) (error 'foxpipe "has to collapse: ~a!" reason)))]
                    [(? input-port?) (match (read /dev/tcpin)
                                       [(? eof-object?) (error 'foxpipe "remote server disconnected!")]
                                       [msgvector (place-channel-put izunac (cons sshd-host msgvector))])])
                  (recv-match-send-loop))))
            (sync/timeout/enable-break (+ (cast (random) Positive-Real) 1.0) never-evt)
            (catch-send-clear-loop)))))))

(provide main)

(require syntax/location)

(define main
  (lambda []
    (define foxpipe (dynamic-place `(submod (file ,(quote-source-file)) foxpipe) 'realize))
    (place-channel-put foxpipe (hasheq))
    (with-handlers ([exn:break? void])
      (let digivice-recv-match-render-loop ()
        (with-handlers ([exn:fail? (lambda [e] (eprintf "~a~n" (string-trim (exn-message e))))])
          (match (sync/enable-break foxpipe)
            [(cons (? string? host) (? flonum? s)) (place-channel-put foxpipe (format "idled ~as" s))]
            [whatever (displayln whatever)]))
        (digivice-recv-match-render-loop)))
    (displayln "Terminating Place")
    (place-break foxpipe 'terminate)
    (sync/enable-break (place-dead-evt foxpipe))))
