#lang typed/racket

(provide (all-defined-out))

(define desc : String "View the real time remote rsyslogs")

(module+ sakuyamon
  (require typed/mred/mred)
  
  (require/typed racket/gui/dynamic
                 [gui-available? (-> Boolean)])

  (require/typed racket
                 [#:opaque TCP-Port tcp-port?])
  
  (require "../../digitama/digicore.rkt")

  (require/typed "../../digitama/tunnel.rkt"
                 [sakuyamon-foxpipe  (-> Thread
                                         TCP-Port
                                         String
                                         Index
                                         [#:username String]
                                         [#:id_rsa.pub Path-String]
                                         [#:id_rsa Path-String]
                                         [#:passphrase String]
                                         Any)])
  
  (define izuna-no-gui? : (Parameterof Boolean) (make-parameter (not (gui-available?))))
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  (define sakuyamon-scepter-host : (Parameterof String) (make-parameter "gyoudmon.org"))
  
  (define cli-main : (-> Any)
    (thunk (let ([izunac (thread (thunk (let ([izunac : Thread (current-thread)])
                                          (define colors : (Listof Term-Color) (list 159 191 223 255))
                                          (let tcp-ssh-channel-connect ()
                                            (define-values [/dev/sshin /dev/sshout] (tcp-connect (cast (sakuyamon-scepter-host) String) 22))
                                            (echof #:fgcolor 'blue "Connected to ~a:~a.~n" (sakuyamon-scepter-host) 22)
                                            (define sshc : Thread (thread (thunk (sakuyamon-foxpipe izunac (cast /dev/sshout TCP-Port)
                                                                                                    "localhost" (sakuyamon-scepter-port)))))
                                            (let poll-channel ([index : Index 0])
                                              (define which (sync/timeout/enable-break (* (sakuyamon-foxpipe-idle) 1.618)
                                                                                       (wrap-evt (thread-dead-evt sshc) (lambda [e] 'collapsed))
                                                                                       (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive)))))
                                              (define msgcolor : Term-Color (list-ref colors index))
                                              (match which
                                                [#false (thread-send sshc 'collapsed) #| tell sshc to terminate |#]
                                                ['collapsed (eechof #:fgcolor 'red "izuna: SSH Tunnel has collapsed!~n")]
                                                [(? list? figureprint) (echof #:fgcolor 'cyan "RSA: ~a~n" figureprint)]
                                                [(? exn:break? signal) (and (thread-send sshc signal) (thread-wait sshc))]
                                                [(? exn? exception) (echof #:fgcolor 'red "~a~n" (exn-message exception))]
                                                [(? box? msgbox) (match (string-split (cast (unbox msgbox) String) #px"\\s+request:\\s+")
                                                                   [(list msg)
                                                                    (unless (string=? msg (string beating-heart#))
                                                                      (echof #:fgcolor msgcolor "~a~n" msg))]
                                                                   [(list msghead reqinfo)
                                                                    (let ([info (cast (with-input-from-string reqinfo read) HashTableTop)])
                                                                      (echof #:fgcolor msgcolor "~a ~a //~a~a [~a ~a] " msghead
                                                                             (hash-ref info 'method)
                                                                             (hash-ref info 'host)
                                                                             (hash-ref info 'uri)
                                                                             (hash-ref info 'client)
                                                                             (hash-ref info 'user-agent #false))
                                                                      (echof #:fgcolor 245 "~s~n"
                                                                             ((inst foldl Symbol HashTableTop Any Any)
                                                                              (lambda [key [info : HashTableTop]] (hash-remove info key)) info
                                                                              '(method host uri user-agent client))))])]
                                                [event (echof #:fgcolor 'yellow "Uncaught Event: ~a~n" event)])
                                              (cond [(exn:break? which) (for-each tcp-abandon-port (list /dev/sshin /dev/sshout))]
                                                    [(thread-dead? sshc) (tcp-ssh-channel-connect)]
                                                    [else (poll-channel (remainder (add1 index) (length colors)))]))))))])
             (with-handlers ([exn:break? (lambda [[signal : exn]] (thread-send izunac signal))])
               (sync/enable-break (thread-dead-evt izunac))))))

  (define gui-main : (-> Any)
    (thunk ((lambda [[digivice% : Frame%]] (send* (make-object digivice% "Loading ..." #false) (show #true) (center 'both)))
            (class frame% (super-new)
              (inherit show center)
              
              (define/override (on-superwindow-show shown?)
                (when shown?
                  (displayln 'here)
                  (send this set-label "Kudagitsune")))))))

  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"--no-gui"} ,(λ [[flag : String]] (izuna-no-gui? #true))
                              {"Do not launch GUI even if available."}]
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        {"Use an alternative <port>." "port"}]])
             (lambda [!flag hostname] (void (sakuyamon-scepter-host hostname) (if (izuna-no-gui?) (cli-main) (gui-main))))
             '{"hostname"}
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
