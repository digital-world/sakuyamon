#lang typed/racket

(provide (all-defined-out))

(require/typed/provide racket
                       [#:opaque TCP-Port tcp-port?])

(require/typed/provide "posix.rkt"
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
                       [rsyslog (-> Symbol Symbol String Void)])

(require/typed/provide "tunnel.rkt"
                       [sakuyamon-foxpipe  (-> Thread
                                               String
                                               Index
                                               TCP-Port
                                               [#:username String]
                                               [#:password (Option String)]
                                               [#:id_rsa.pub Path-String]
                                               [#:id_rsa Path-String]
                                               [#:passphrase String]
                                               Any)])
