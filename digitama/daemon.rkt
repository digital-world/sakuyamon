#lang typed/racket

(require/typed/provide "posix.rkt"
                       [saved-errno (-> Natural)]
                       [strerror (-> Natural String)]
                       [getuid (-> Natural)]
                       [getgid (-> Natural)]
                       [geteuid (-> Natural)]
                       [getegid (-> Natural)]
                       [seteuid (-> Natural Integer)]
                       [setegid (-> Natural Integer)]
                       [fetch_tamer_ids (-> Bytes (Values Natural Natural Natural))]
                       [fetch_tamer_name (-> Natural (Values Natural Bytes))]
                       [fetch_tamer_group (-> Natural (Values Natural Bytes))]
                       [severity.c (-> Symbol Integer)]
                       [severity.rkt (-> Integer Symbol)]
                       [rsyslog (-> Integer Symbol String Void)]
                       [syslog (-> Symbol Symbol String Any * Void)])
