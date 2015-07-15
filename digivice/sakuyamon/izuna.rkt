#lang typed/racket

(provide (all-defined-out))

(define desc : String "View the real time remote rsyslogs")

(module+ sakuyamon
  (require typed/mred/mred)
  
  (require/typed racket/gui/dynamic
                 [gui-available? (-> Boolean)])
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/geolocation.rkt")

  (require/typed "../../digitama/tunnel.rkt"
                 [sakuyamon-foxpipe  (-> Thread
                                         String
                                         String
                                         Index
                                         [#:username String]
                                         [#:id_rsa.pub Path-String]
                                         [#:id_rsa Path-String]
                                         [#:passphrase String]
                                         Any)])
  
  (define izuna-no-gui? : (Parameterof Boolean) (make-parameter (not (gui-available?))))
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  (define sakuyamon-scepter-hosts : (Parameterof (Listof String)) (make-parameter null))

  (define ~geolocation : (-> String String)
    (lambda [ip]
      (define geo : Maybe-Geolocation (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))])))
  
  (define cli-main : (-> Any)
    (lambda []
      (define open-box : (-> BoxTop Term-Color Char Void)
        (lambda [msgbox msgcolor heart]
          (match (string-split (cast (unbox msgbox) String) #px"\\s+request:\\s+")
            [(list msg)
             (cond [(string=? (string beating-heart#) msg)
                    (void (printf "\033[s\033[K\033[2C\033[38;5;~am~a\033[0m\033[u" msgcolor heart)
                          (flush-output (current-output-port)))]
                   [(regexp-match* #px"\\d+(\\.\\d+){3}(?!\\.\\S)" msg)
                    => (lambda [[ips : (Listof String)]]
                         (echof #:fgcolor msgcolor "~a~n"
                                (regexp-replaces msg (map (lambda [[ip : String]] (list ip (~geolocation ip))) ips))))]
                   [else (echof #:fgcolor msgcolor "~a~n" msg)])]
            [(list msghead reqinfo)
             (let ([info (cast (with-input-from-string reqinfo read) HashTableTop)])
               (echof #:fgcolor msgcolor "~a ~a@~a //~a~a #\"~a\" " msghead
                      (hash-ref info 'method) (~geolocation (cast (hash-ref info 'client) String))
                      (hash-ref info 'host #false) (hash-ref info 'uri)
                      (hash-ref info 'user-agent #false))
               (echof #:fgcolor 245 "~s~n"
                      ((inst foldl Symbol HashTableTop Any Any)
                       (lambda [key [info : HashTableTop]] (hash-remove info key)) info
                       '(method host uri user-agent client))))])))
      (define izunacs : (Listof Thread)
        (build-list (length (sakuyamon-scepter-hosts))
                    (lambda [[i : Index]]
                      (define scepter-host : String (list-ref (sakuyamon-scepter-hosts) i))
                      (thread (thunk (let ([izunac : Thread (current-thread)])
                                       (define colors : (Listof Term-Color) (list 159 191 223 255))
                                       (define hearts : (Listof Char) (list beating-heart# two-heart# sparkling-heart# growing-heart# arrow-heart#))
                                       (let tcp-ssh-channel-connect ()
                                         (define sshc : Thread
                                           (thread (thunk (void (echof #:fgcolor 'blue "Connecting to ~a:~a.~n" scepter-host 22)
                                                                (sakuyamon-foxpipe izunac scepter-host "localhost" (sakuyamon-scepter-port))))))
                                         (let poll-channel ()
                                           (define which (sync/timeout/enable-break (* (sakuyamon-foxpipe-idle) 1.618)
                                                                                    (wrap-evt (thread-dead-evt sshc) (lambda [e] 'collapsed))
                                                                                    (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive)))))
                                           (define ci : Index (cast (random (length colors)) Index))
                                           (define hi : Index (cast (random (length hearts)) Index))
                                           (match which
                                             [#false (thread-send sshc 'collapsed) #| tell sshc to terminate |#]
                                             ['collapsed (eechof #:fgcolor 'red "izuna: Tunnel@~a has collapsed!~n" scepter-host)]
                                             [(? list? figureprint) (echof #:fgcolor 'cyan "RSA[~a]: ~a~n" scepter-host figureprint)]
                                             [(? exn:break? signal) (thread-send sshc signal)]
                                             [(? exn? exception) (echof #:fgcolor 'yellow "~a: ~a~n" scepter-host (exn-message exception))]
                                             [(? box? msgbox) (open-box msgbox (list-ref colors ci) (list-ref hearts hi))])
                                           (cond [(exn:break? which) (thread-wait sshc)]
                                                 [(thread-dead? sshc) (tcp-ssh-channel-connect)]
                                                 [else (poll-channel)])))))))))
      (with-handlers ([exn:break? (lambda [[signal : exn]] (for-each (lambda [[t : Thread]] (thread-send t signal)) izunacs))])
        (let exit-if-failed-all ()
          (define living-izunacs : (Listof Thread) (filter-not thread-dead? izunacs))
          (unless (null? living-izunacs)
            (apply sync/enable-break (map thread-dead-evt living-izunacs))
            (exit-if-failed-all))))
      (for-each thread-wait izunacs)))

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
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String * Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"--no-gui"} ,(λ [[flag : String]] (izuna-no-gui? #true))
                              {"Do not launch GUI even if available."}]
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        {"Use an alternative service <port>." "port"}]])
             (lambda [!flag . hostnames] (void (sakuyamon-scepter-hosts hostnames) (if (izuna-no-gui?) (cli-main) (gui-main))))
             '{"hostname"}
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
