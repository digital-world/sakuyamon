#lang at-exp typed/racket

@require{../../DigiGnome/digitama/digicore.rkt}

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/digicore.rkt"))

(current-digimon "sakuyamon")

(define info-ref : (Option Info-Ref) (get-info/full (digimon-zone)))

(define sakuyamon-user-terminus? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-terminus-user? {λ _ #false}) Boolean)))
(define sakuyamon-digimon-terminus? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-terminus-digimon? {λ _ #false}) Boolean)))

(define sakuyamon-ssl? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-ssl? {λ _ #false}) Boolean)))
(define sakuyamon-port : (Parameterof (Option Natural))
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-port {λ _ #false}) (Option Natural))))
(define sakuyamon-max-waiting : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-max-waiting {λ _ 511}) Natural)))
(define sakuyamon-connection-timeout : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-connection-timeout {λ _ 30}) Natural)))

(define sakuyamon-timeout-default-servlet : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-timeout-default-servlet {λ _ 30}) Natural)))
(define sakuyamon-timeout-servlet-connection : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-timeout-servlet-connection {λ _ 86400}) Natural)))
