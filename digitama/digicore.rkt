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
(define sakuyamon-port : (Parameterof (Option Natural))
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-port (const #false)) (Option Natural))))
(define sakuyamon-max-waiting : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-max-waiting (const 511)) Natural)))
(define sakuyamon-connection-timeout : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-config-connection-timeout (const 30)) Natural)))

(define sakuyamon-timeout-default-servlet : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-timeout-default-servlet (const 30)) Natural)))
(define sakuyamon-timeout-servlet-connection : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-timeout-servlet-connection (const 86400)) Natural)))

(define sakuyamon-foxpipe-port : (Parameterof (Option Natural))
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-foxpipe-port (const #false)) (Option Natural))))
(define sakuyamon-foxpipe-max-waiting : (Parameterof Natural)
  (make-parameter (cast ((cast info-ref Info-Ref) 'sakuyamon-foxpipe-max-waiting (const 4)) Natural)))
