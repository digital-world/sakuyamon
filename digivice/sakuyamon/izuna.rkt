#lang typed/racket

(provide (all-defined-out))

(define desc : String "View the real time remote rsyslogs")

(module+ sakuyamon
  (require/typed racket/gui/dynamic
                 [gui-dynamic-require (-> Symbol Any)])

  (require/typed racket
                 [#:opaque TCP-Port tcp-port?])

  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/agent.rkt")

  (require/typed "../../digitama/tunnel.rkt"
                 [sakuyamon-foxpipe  (-> Thread
                                         String
                                         Index
                                         TCP-Port
                                         [#:password (Option String)]
                                         [#:id_rsa.pub Path-String]
                                         [#:id_rsa Path-String]
                                         [#:passphrase String]
                                         Any)])
  
  (define sakuyamon-scepter-host : (Parameterof String) (make-parameter "gyoudmon.org"))
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))

  (define main : (-> Any)
    (thunk (parameterize ([current-custodian (make-custodian)])
             (define izunac : Thread (current-thread))
             (define sshc : (Parameterof (Option Thread)) (make-parameter #false))
             (with-handlers ([exn:break? (lambda [[signal : exn]] (let ([t (sshc)]) (when (thread? t) (break-thread t))))])
               (define-values [/dev/sshin /dev/sshout] (tcp-connect (cast (sakuyamon-scepter-host) String) 22))
               (printf "connected to ~a:~a.~n" (sakuyamon-scepter-host) 22)
               (sshc (thread (thunk (sakuyamon-foxpipe izunac "localhost" (sakuyamon-scepter-port) (cast /dev/sshout TCP-Port)))))
               (let poll ()
                 (match (sync/timeout/enable-break (* (sakuyamon-foxpipe-idle) 1.618)
                                                   (wrap-evt (thread-dead-evt (cast (sshc) Thread)) (const 'collapsed))
                                                   (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive))))
                   [#false (let ([t (cast (sshc) Thread)]) (unless (thread-dead? t) (break-thread t)))]
                   ['collapsed (eprintf "tunnel has collapsed: ~a~n" (thread-receive))]
                   [(cons figureprint authlist) (printf "~a~n~a~n" figureprint authlist)]
                   [(? box? msgbox) (printf "~a~n" (unbox msgbox))]
                   [(? exn? exception) (displayln exception)]
                   [event (printf "Uncaught Event: ~a~n" event)])
                 (unless (thread-dead? (cast (sshc) Thread)) (poll)))))))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        {"Use an alternative <port>." "port"}]])
             (lambda [!flag hostname] (and (sakuyamon-scepter-host hostname) (main)))
             '{"hostname"}
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
