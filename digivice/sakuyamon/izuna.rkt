#lang typed/racket

(provide (all-defined-out))

(define desc : String "Monitor rsyslogs via SSH Tunnel")

(module+ sakuyamon
  (require/typed racket/generator
                 [sequence->repeated-generator (All [a] (-> (Sequenceof a) (-> a)))])
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/geolocation.rkt")
  
  (define sakuyamon-scepter-port : (Parameterof Index) (make-parameter (sakuyamon-foxpipe-port)))
  
  (define foxpipes : (HashTable String Place) (make-hash))
  (define msgcolor : (-> Term-Color)
    ((inst sequence->repeated-generator Term-Color) (list 123 155 187 159 191 223 255)))
  (define heart : (-> Char)
    ((inst sequence->repeated-generator Char) (list beating-heart# two-heart# sparkling-heart# growing-heart# arrow-heart#)))
  
  (define ~geolocation : (-> String String)
    (lambda [ip]
      (define geo : Maybe-Geolocation (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))])))
  
  (define build-tunnel : (-> String Void)
    (lambda [scepter-host]
      (define foxpipe : Place (dynamic-place (build-path (digimon-digitama) "foxpipe.rkt") 'sakuyamon-foxpipe))
      (place-channel-put foxpipe (hash 'sshd-host scepter-host
                                       'host-seen-by-sshd "localhost"
                                       'service-seen-by-sshd (sakuyamon-scepter-port)))
      (hash-set! foxpipes scepter-host foxpipe)))
  
  (define px.filter : (Listof Regexp)
    (list #px"\\S+\\[\\d+\\]:\\s*$" #| empty-messaged log such as Safari[xxx] |#
          #px"taskgated\\[\\d+\\]:" #| all especially "no system signature for unsigned ..." |#))
  
  (define fold-message : (-> Term-Color Any Void)
    (lambda [msgcolour message]
      (printf "\033[s\033[2C\033[38;5;~am~a\033[0m\033[u" msgcolour message)))
  
  (define print-message : (-> String Any Void)
    (lambda [scepter-host message]
      (printf "\033[K") ; clear cursor line
      (cond [(equal? message beating-heart#) (fold-message (msgcolor) (heart))]
            [(list? message) (for-each (curry print-message scepter-host) message)] ;;; single-line message is also (list)ed.
            [(string? message) (match (string-split message #px"\\s+request:\\s+")
                                 [(list msg)
                                  (cond [(ormap (lambda [[px : Regexp]] (regexp-match px msg)) px.filter) (fold-message 245 msg)]
                                        [(regexp-match* #px"\\d+(\\.\\d+){3}(?!\\.\\S)" msg)
                                         => (lambda [[ips : (Listof String)]]
                                              (echof #:fgcolor (msgcolor) "~a~n"
                                                     (regexp-replaces msg (map (lambda [[ip : String]] (list ip (~geolocation ip))) ips))))]
                                        [else (echof #:fgcolor msgcolor "~a~n" msg)])]
                                 [(list msghead reqinfo)
                                  (let ([info (cast (with-input-from-string reqinfo read) HashTableTop)])
                                    (echof #:fgcolor (msgcolor) "~a ~a@~a //~a~a #\"~a\" " msghead
                                           (hash-ref info 'method) (~geolocation (cast (hash-ref info 'client) String))
                                           (hash-ref info 'host #false) (hash-ref info 'uri)
                                           (hash-ref info 'user-agent #false))
                                    (echof #:fgcolor 245 "~s~n"
                                           ((inst foldl Symbol HashTableTop Any Any)
                                            (lambda [key [info : HashTableTop]] (hash-remove info key)) info
                                            '(method host uri user-agent client))))])]
            [else (echof #:fgcolor 245 "Unexpected Message from ~a: ~a(~s)~n" scepter-host message message)])
      (flush-output (current-output-port))))
  
  (define monitor-main : (-> String * Any)
    (lambda hostnames
      (for-each build-tunnel hostnames)
      (define on-signal : (-> exn Void)
        (lambda [signal]
          (newline)
          (for-each (lambda [[foxpipe : Place]] (place-break foxpipe 'terminate)) (hash-values foxpipes))
          (let wait-channel ()
            (define who (apply sync (hash-map foxpipes (lambda [[host : String] [foxpipe : Place]]
                                                         (wrap-evt (place-dead-evt foxpipe) (const host))))))
            (hash-remove! foxpipes who)
            (echof #:fgcolor 'blue "~a: Foxpipe has collapsed~n" who)
            (unless (zero? (hash-count foxpipes)) (wait-channel)))))
      (with-handlers ([exn:break? on-signal])
        (let poll-channel ()
          (match (apply sync/enable-break (hash-values foxpipes))
            [(cons host (vector message)) (print-message (cast host String) message)]
            [(cons host (? flonum? s)) (place-channel-put (cast (hash-ref foxpipes host) Place) (format "idled ~as" s))]
            [(list host 'fail message) (echof #:fgcolor 'red "~a: ~a~n" host message)]
            [(list host (? string? figureprint) ...) (echof #:fgcolor 'cyan "~a: RSA: ~a~n" host figureprint)]
            [(list host 'notify (? string? fmt) argl ...) (echof #:fgcolor 'blue "~a: ~a~n" host (apply format fmt argl))])
          (poll-channel)))))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String * Any)
                                          (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each
                [{"-p"} ,(λ [[flag : String] [port : String]] (sakuyamon-scepter-port (cast (string->number port) Index)))
                        {"Use an alternative service <port>." "port"}]])
             (lambda [!flag . hostnames] (apply monitor-main hostnames))
             '{"hostname"} ;;; Although it can watch multihosts at the same time, but this usage is not recommended due to poor (sync)
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
