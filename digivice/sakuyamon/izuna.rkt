#lang typed/racket

(provide (all-defined-out))

(define desc : String "View the real time remote rsyslogs")

(module+ sakuyamon
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/agent.rkt")
  
  (define sakuyamon-scepter-host : (Parameterof String) (make-parameter "gyoudmon.org"))
  (define sakuyamon-scepter-port : (Parameterof Positive-Integer) (make-parameter (sakuyamon-foxpipe-port)))

  (define sakuyamon-curl : (-> String * Client-Response)
    (lambda arglist
      (apply sakuyamon-agent "localhost" (cast (or (sakuyamon-port) 80) Positive-Integer) arglist)))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String * Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Positive-Integer)))
                        {"Use an alternative <port>." "port"}]])
             (lambda [!flag . argl]
               (parameterize ([current-custodian (make-custodian)])
                 (unless (null? argl) (sakuyamon-scepter-host (cast (car argl) String)))
                 (with-handlers ([exn:break? void]
                                 [exn? displayln])
                   (let reconnect ()
                     (match-define {cons /dev/tcpin /dev/tcpout}
                       (foxpipe-connect (sakuyamon-scepter-host) (sakuyamon-scepter-port)))
                     (printf "connected to ~a:~a.~n" (sakuyamon-scepter-host) (sakuyamon-scepter-port))
                     (let pull ()
                       (match (sync/timeout/enable-break (* (sakuyamon-foxpipe-idle) 1.618) /dev/tcpin)
                         [#false (and (eprintf "connection has disconnected. retry later...~n")
                                      (tcp-abandon-port /dev/tcpin)
                                      (tcp-abandon-port /dev/tcpout)
                                      (reconnect))]
                         [else (let ([v (read /dev/tcpin)])
                                 (unless (equal? v beating-heart#)
                                   (displayln v))
                                 (if (eof-object? v)
                                     (reconnect)
                                     (pull)))]))))))
             '{"hostname"}
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
