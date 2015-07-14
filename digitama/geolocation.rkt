#lang typed/racket

(provide (all-defined-out))

(require "agent.rkt")

(struct geolocation ([continent : String]
                     [country : String]
                     [state/region : (Option String)]
                     [city : (Option String)]
                     [latitude : String]
                     [longitude : String])
  #:transparent)

(define-type Geolocation geolocation)
(define-type Maybe-Geolocation (Option geolocation))

(define what-is-my-address : (-> String Maybe-Geolocation)
  (let ([geodb : (HashTable String Maybe-Geolocation) (make-hash)])
    (lambda [ip]
      (with-handlers ([void (const #false)])
        (unless (hash-has-key? geodb ip)
          (match-define (list a b c d) (sakuyamon-agent "whatismyipaddress.com" 80 "-L" (~a "/ip/" ip)))
          (define table (regexp-match #px"<table>\\s*<tr><th>Continent:.*?</table>" d))
          ((inst hash-set! String Maybe-Geolocation)
           geodb ip
           (and (list? table)
                (match (string-split (cast (regexp-replaces (bytes->string/utf-8 (car table))
                                                            '([#px"(&nbsp;)+" " "] [#px"&deg;" "°"]
                                                              [#px"&prime;" "′"] [#px"&Prime;" "′′"])) String)
                                     #px"(:?\\s*<[^>]+>\\s*)+")
                  [(list "Continent" continent "Country" country "State/Region" state "City" city "Latitude" latitude "Longitude" longitude whocares ...)
                   (geolocation continent country state city latitude longitude)]
                  [(list "Continent" continent "Country" country "Latitude" latitude "Longitude" longitude whocare ...)
                   (geolocation continent country #false #false latitude longitude)]
                  [what-is-wrong #false]))))
        ((inst hash-ref String Maybe-Geolocation False) geodb ip (const #false))))))

(module+ test
  (require typed/net/dns)

  (define ipv4 (dns-get-address "8.8.8.8" "gyoudmon.org"))
  (define ipv6 (dns-get-address "8.8.8.8" "gyoudmon.org" #:ipv6? #true))
  
  (what-is-my-address ipv4)
  (what-is-my-address ipv6)

  (what-is-my-address "93.195.192.224"))