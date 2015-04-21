#lang at-exp typed/racket

@require{../../DigiGnome/digitama/digicore.typed.rkt}

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/digicore.typed.rkt"))

(current-digimon "sakuyamon")

(define info-ref : (Option Info-Ref) (get-info/full (digimon-zone)))

(define sakuyamon-ssl? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-ssl? {λ _ #false}) Boolean)))
(define sakuyamon-port : (Parameterof (Option Positive-Index))
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-port {λ _ #false}) (Option Positive-Index))))
(define sakuyamon-max-waiting : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-max-waiting {λ _ 511}) Natural)))
(define sakuyamon-connection-timeout : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-connection-timeout {λ _ 30}) Natural)))
  
(define sakuyamon-webroot : {-> Path-String} (cast (procedure-rename digimon-terminus 'sakuyamon-webroot) {-> Path-String}))
(define sakuyamon-config : {-> Path-String} (cast (procedure-rename digimon-stone 'sakuyamon-config) {-> Path-String}))
