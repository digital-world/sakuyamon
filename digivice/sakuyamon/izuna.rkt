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
                                         Nonnegative-Flonum
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

  (define-type Tunnel-Dead-Evt (Evtof (Pairof String Symbol)))
  
  (define sshcs : (HashTable String Thread) (make-hash))
  (define on-sshcs : (HashTable String Tunnel-Dead-Evt) (make-hash))

  (define ~geolocation : (-> String String)
    (lambda [ip]
      (define geo : Maybe-Geolocation (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))])))

  (define build-tunnel : (-> Thread Thread-Group (-> String Any) String Void)
    (lambda [izunac sshc-group echo-connection scepter-host]
      (echo-connection scepter-host)
      (parameterize ([current-thread-group sshc-group])
        (define sshc : Thread (thread (thunk (sakuyamon-foxpipe izunac (* (sakuyamon-foxpipe-idle) 1.618)
                                                                scepter-host "localhost" (sakuyamon-scepter-port)))))
        (hash-set! sshcs scepter-host sshc)
        (hash-set! on-sshcs scepter-host (wrap-evt (thread-dead-evt sshc) (const (cons scepter-host 'collapsed)))))))

  (define cli-main : (-> Any)
    (lambda []
      (define open-box : (-> String BoxTop Term-Color Char Void)
        (lambda [scepter-host msgbox msgcolor heart]
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
      (define izunac : Thread
        (thread (thunk (let ([izunac : Thread (current-thread)]
                             [sshc-group : Thread-Group (make-thread-group)])
                         (define colors : (Listof Term-Color) (list 123 155 187 219 159 191 223 255))
                         (define hearts : (Listof Char) (list beating-heart# two-heart# sparkling-heart# growing-heart# arrow-heart#))
                         (define on-syslog : (Evtof Any) (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive))))
                         (define echo-connection : (-> String Any) (lambda [host] (echof #:fgcolor 'blue "Connecting to ~a:~a.~n" host 22)))
                         (for-each (lambda [[host : String]] (build-tunnel izunac sshc-group echo-connection host)) (sakuyamon-scepter-hosts))
                         (let poll-channel ()
                           (define which (apply sync on-syslog (hash-values on-sshcs)))
                           (define ci : Index (cast (random (length colors)) Index))
                           (define hi : Index (cast (random (length hearts)) Index))
                           (match which
                             [(cons host (? box? msgbox)) (open-box (cast host String) msgbox (list-ref colors ci) (list-ref hearts hi))]
                             [(cons host (? exn? exception)) (echof #:fgcolor 'red "~a: ~a~n" host (exn-message exception))]
                             [(cons host 'collapsed) (build-tunnel izunac sshc-group echo-connection (cast host String))]
                             [(? exn:break? signal) (hash-map sshcs (lambda [[who : String] [sshc : Thread]] (thread-send sshc signal)))]
                             [(cons host (? flonum? s)) (thread-send (cast (hash-ref sshcs host) Thread) (cons 'collapse (format "idled ~as" s)))]
                             [(list host figureprint ...) (echof #:fgcolor 'cyan "RSA[~a]: ~a~n" host figureprint)]
                             [reason (hash-map sshcs (lambda [[who : String] [sshc : Thread]] (thread-send sshc (cons 'collapse reason))))])
                           (cond [(exn:break? which) (hash-map sshcs (lambda [[who : String] [sshc : Thread]] (thread-wait sshc)))]
                                 [else (poll-channel)]))))))
      (with-handlers ([exn:break? (lambda [[signal : exn]] (thread-send izunac signal))])
        (sync/enable-break (thread-dead-evt izunac)))))

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
