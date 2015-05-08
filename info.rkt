#lang info

(define collection "Sakuyamon")

(define version "Baby")

(define pkg-desc "Manage and protect http://gyoudmon.org.")

(define build-deps '{"base" "typed-racket-lib" "scribble-lib" "web-server-lib"})

(define compile-omit-paths (list "stone" "info.rkt"))
(define test-omit-paths 'all)

(define racket-launcher-names (list "sakuyamon"))
(define racket-launcher-libraries (list "digivice/sakuyamon.rkt"))

(define sakuyamon-terminus-user? #true)
(define sakuyamon-terminus-digimon? #true)

(define sakuyamon-config-ssl? #false)
(define sakuyamon-config-port #false)
(define sakuyamon-config-max-waiting 511)
(define sakuyamon-config-connection-timeout 30)

(define sakuyamon-timeout-default-servlet 30)
(define sakuyamon-timeout-password 300)
(define sakuyamon-timeout-servlet-connection 86400)
(define sakuyamon-timeout-file/byte 1/20)
(define sakuyamon-timeout-file 30)
