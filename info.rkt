#lang info

(define collection "Sakuyamon")

(define version "Baby I")

(define pkg-desc "Be in charge of [gyoudmon.org](http://gyoudmon.org).")

(define compile-omit-paths (list "makefile.rkt" "info.rkt" "stone"))

(define skymn-config-port #false)
(define skymn-config-ssl #false)
(define skymn-config-max-waiting 511)

(define skymn-timeout-initial-connection 30)
(define skymn-timeout-default 30)
(define skymn-timeout-password 300)
(define skymn-timeout-servlet 86400)
(define skymn-timeout-file/byte 1/20)
(define skymn-timeout-file 30)
