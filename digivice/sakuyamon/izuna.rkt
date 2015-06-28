#lang typed/racket

(provide (all-defined-out))

(define desc : String "View the real time remote rsyslogs")

(module+ sakuyamon
  (require "../../digitama/digicore.rkt")
  
  (define sakuyamon-scepter-host : (Parameterof String) (make-parameter "gyoudmon.org"))
  (define sakuyamon-scepter-port : (Parameterof Natural) (make-parameter (or (sakuyamon-foxpipe-port) 514)))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String * Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `{{usage-help ,(format "~a~n" desc)}
               {once-each [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Natural)))
                                  {"Use an alternative <port>." "port"}]}}
             (lambda [!flag . argl]
               (parameterize ([current-custodian (make-custodian)])
                 (unless (null? argl) (sakuyamon-scepter-host (cast (car argl) String)))
                 (with-handlers ([exn:break? void]
                                 [exn? displayln])
                   (define-values {/dev/tcpin /dev/tcpout} (tcp-connect/enable-break (sakuyamon-scepter-host) (sakuyamon-scepter-port)))
                   (let pull ()
                     (sync /dev/tcpin)
                     (displayln (read /dev/tcpin))
                     (pull)))))
             '{"hostname"}
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))