#lang typed/racket

(provide (all-defined-out))

(define desc : String "View the real time remote rsyslogs")

(module+ sakuyamon
  (require typed/mred/mred)
  
  (require/typed racket/gui/dynamic
                 [gui-available? (-> Boolean)])
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/geolocation.rkt")

  (define izuna-no-gui? : (Parameterof Boolean) (make-parameter (not (gui-available?))))
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  (define sakuyamon-scepter-hosts : (Parameterof (Listof String)) (make-parameter null))

  (define-type Tunnel-Dead-Evt (Evtof (Pairof String Symbol)))
  
  (define sshcs : (HashTable String Place) (make-hash))

  (define ~geolocation : (-> String String)
    (lambda [ip]
      (define geo : Maybe-Geolocation (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))])))

  (define build-tunnel : (-> String Void)
    (lambda [scepter-host]
      (define sshc : Place (dynamic-place (build-path (digimon-digitama) "foxpipe.rkt") 'sakuyamon-foxpipe))
      (place-channel-put sshc (hash 'timeout (* (sakuyamon-foxpipe-idle) 1.618)
                                    'sshd-host scepter-host
                                    'host-seen-by-sshd "localhost"
                                    'service-seen-by-sshd (sakuyamon-scepter-port)))
      (hash-set! sshcs scepter-host sshc)))

  (define cli-main : (-> Any)
    (lambda []
      (define colors : (Listof Term-Color) (list 123 155 187 159 191 223 255))
      (define hearts : (Listof Char) (list beating-heart# two-heart# sparkling-heart# growing-heart# arrow-heart#))
      (define print-message : (-> String Any Void)
        (lambda [scepter-host message]
          (define msgcolor : Term-Color (list-ref colors (cast (random (length colors)) Index)))
          (define heart : Char (list-ref hearts (cast (random (length hearts)) Index)))
          (cond [(equal? message beating-heart#) (printf "\033[s\033[K\033[2C\033[38;5;~am~a\033[0m\033[u" msgcolor heart)]
                [(list? message) (for-each (curry print-message scepter-host) message)] ;;; single-line message is also (list)ed.
                [(string? message) (match (string-split message #px"\\s+request:\\s+")
                                     [(list msg)
                                      (cond [(regexp-match #px"\\S+\\[\\d+\\]:\\s*$" msg)
                                             (void 'skip) #| empty-messaged log such as Safari[xxx]: |#]
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
                                                '(method host uri user-agent client))))])]
                [else (echof #:fgcolor 245 "Unexpected Message: ~s~n" message)])
          (flush-output (current-output-port))))
      (for-each build-tunnel (sakuyamon-scepter-hosts))
      (define on-signal : (-> exn Void)
        (lambda [signal]
          (newline)
          (for-each (lambda [[sshc : Place]] (place-break sshc 'terminate)) (hash-values sshcs))
          (let wait-channel ()
            (define who (apply sync (hash-map sshcs (lambda [[host : String] [sshc : Place]]
                                                      (wrap-evt (place-dead-evt sshc) (const host))))))
            (hash-remove! sshcs who)
            (echof #:fgcolor 'blue "~a: SSH Tunnel has collapsed~n" who)
            (unless (zero? (hash-count sshcs)) (wait-channel)))))
      (with-handlers ([exn:break? on-signal])
        (let poll-channel ()
          (match (apply sync/enable-break (hash-values sshcs))
            [(cons host (vector message)) (print-message (cast host String) message)]
            [(cons host (? flonum? s)) (place-channel-put (cast (hash-ref sshcs host) Place) (format "idled ~as" s))]
            [(list host 'fail message) (echof #:fgcolor 'red "~a: ~a~n" host message)]
            [(list host (? string? figureprint) ...) (echof #:fgcolor 'cyan "~a: RSA: ~a~n" host figureprint)]
            [(list host 'notify (? string? fmt) argl ...) (echof #:fgcolor 'blue "~a: ~a~n" host (apply format fmt argl))])
          (poll-channel)))))

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
             '{"hostname"} ;;; Although it can watch multihosts at the same time, but this usage is not recommended due to poor (sync)
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
