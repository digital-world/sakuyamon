#lang at-exp typed/racket

@require{../../DigiGnome/digitama/digicore.rkt}

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/digicore.rkt"))

(current-digimon "sakuyamon")

(define info-ref : (Option Info-Ref) (get-info/full (digimon-zone)))

(define sakuyamon-tamer-terminus? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-terminus-tamer? (const #false)) Boolean)))
(define sakuyamon-digimon-terminus? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-terminus-digimon? (const #false)) Boolean)))

(define sakuyamon-ssl? : (Parameterof Boolean)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-ssl? (const #false)) Boolean)))
(define sakuyamon-port : (Parameterof (Option Index))
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-port (const #false)) (Option Index))))
(define sakuyamon-max-waiting : (Parameterof Positive-Integer)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-max-waiting (const 511)) Positive-Integer)))
(define sakuyamon-connection-timeout : (Parameterof Positive-Integer)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-connection-timeout (const 30)) Positive-Integer)))

(define sakuyamon-timeout-default-servlet : (Parameterof Positive-Integer)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-timeout-default-servlet (const 30)) Positive-Integer)))
(define sakuyamon-timeout-servlet-connection : (Parameterof Positive-Integer)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-timeout-servlet-connection (const 86400)) Positive-Integer)))

(define sakuyamon-foxpipe-port : (Parameterof Index)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-foxpipe-port (const 514)) Index)))
(define sakuyamon-foxpipe-idle : (Parameterof Positive-Real)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-foxpipe-timeout-idle (const 8.0)) Positive-Real)))
(define sakuyamon-foxpipe-max-waiting : (Parameterof Positive-Integer)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-foxpipe-max-waiting (const 4)) Positive-Integer)))
