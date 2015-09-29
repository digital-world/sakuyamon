#lang at-exp typed/racket

@require{../../DigiGnome/digitama/digicore.rkt}

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/digicore.rkt"))

(current-digimon "sakuyamon")

(define-parameter/extract-info (digimon-zone)
  [[sakuyamon-tamer-terminus? sakuyamon-terminus-tamer?] : Boolean = #false]
  [[sakuyamon-digimon-terminus? sakuyamon-terminus-digimon?] : Boolean = #false]

  [[sakuyamon-ssl? sakuyamon-config-ssl?] : Boolean = #false]
  [[sakuyamon-port sakuyamon-terminus-port] : (Option Index) = #false]
  [[sakuyamon-max-waiting sakuyamon-config-max-waiting] : Positive-Index = 511]
  [[sakuyamon-connection-timeout sakuyamon-config-connnection-timeout] : Positive-Index = 30]

  [sakuyamon-timeout-default-servlet : Positive-Integer = 30]
  [sakuyamon-timeout-servlet-connection : Positive-Integer = 86400])

(define-parameter/extract-info (digimon-zone)
  [sakuyamon-foxpipe-port : Index = 514]
  [sakuyamon-foxpipe-max-waiting : Positive-Integer = 4]
  [sakuyamon-foxpipe-sampling-interval : Positive-Real = 0.618])
