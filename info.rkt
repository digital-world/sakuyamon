#lang info

(define collection "Sakuyamon")

(define version "Baby")

(define pkg-desc "Manage and protect http://gyoudmon.org.")

(define compile-omit-paths (list "stone" "info.rkt"))
(define test-omit-paths 'all)

(define racket-launcher-names (list "sakuyamon"))
(define racket-launcher-libraries (list "digivice/sakuyamon.rkt"))

(define sakuyamon-config-ssl? #false)
(define sakuyamon-config-port #false)
(define sakuyamon-config-max-waiting 511)
(define sakuyamon-config-connection-timeout 30)

(define sakuyamon-timeout-default-servlet 30)
(define sakuyamon-timeout-password 300)
(define sakuyamon-timeout-servlet-connection 86400)
(define sakuyamon-timeout-file/byte 1/20)
(define sakuyamon-timeout-file 30)

#|
'((port 80)
  (max-waiting 511)
  (initial-connection-timeout 30)
  (default-host-table
    (host-table
     (default-indices "index.html" "index.htm")
     (log-format parenthesized-default)
     (messages
      (servlet-message "servlet-error.html")
      (authentication-message "forbidden.html")
      (servlets-refreshed "servlet-refresh.html")
      (passwords-refreshed "passwords-refresh.html")
      (file-not-found-message "not-found.html")
      (protocol-message "protocol-error.html")
      (collect-garbage "collect-garbage.html"))
     (timeouts
      (default-servlet-timeout 30)
      (password-connection-timeout 300)
      (servlet-connection-timeout 86400)
      (file-per-byte-connection-timeout 1/20)
      (file-base-connection-timeout 30))
     (paths
      (configuration-root "conf")
      (host-root ".")
      (log-file-path "log")
      (file-root "htdocs")
      (servlet-root ".")
      (mime-types "mime.types")
      (password-authentication "passwords"))))
  (virtual-host-table))
|#
