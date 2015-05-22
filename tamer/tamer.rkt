#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../digitama/digicore.rkt}
@require{../../DigiGnome/digitama/tamer.rkt}

(require (submod "../digivice/sakuyamon/realize.rkt" digitama))

(require file/md5)
(require net/head)
(require net/base64)
(require net/http-client)
(require web-server/http)
(require web-server/test)

(provide (all-defined-out))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))
(provide (all-from-out net/head net/base64 net/http-client web-server/http web-server/test))

(define whoami (getenv "USER"))
(define /htdocs (curry format "/~a"))
(define /tamer (curry format "/~~~a/~a" whoami))
(define /digimon (curry format "/~~~a:~a/~a" whoami (current-digimon)))

(define ~htdocs (curry build-path (digimon-terminus)))
(define ~tamer (curry build-path (expand-user-path (format "~~~a" whoami)) "DigitalWorld" "Kuzuhamon" "terminus"))
(define ~digimon (curry build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook"))

(define realm.rktd (path->string (build-path (digimon-stone) "realm.rktd")))

(define sakuyamon-realize
  {lambda arglist
    (parameterize ([current-custodian (make-custodian)])
      (define-values {sakuyamon place-in place-out place-err}
        {place* #:in #false #:out #false #:err #false pipe
                (parameterize ([current-command-line-arguments (place-channel-get pipe)]
                               [tamer-pipe pipe])
                  (dynamic-require `(submod ,(build-path (digimon-digivice) "sakuyamon/realize.rkt") sakuyamon) #false))})
      (place-channel-put sakuyamon (list->vector (if (member "-p" arglist) arglist (list* "-p" "0" arglist))))
      (define {shutdown #:kill? [kill? #false]}
        (let ([sakuyamon-zone (current-custodian)]) ; place is also managed by custodian
          {λ _ (dynamic-wind {λ _ (close-output-port place-in)}
                             {λ _ (and (when kill? (place-kill sakuyamon))
                                       (place-wait sakuyamon))}
                             {λ _ (custodian-shutdown-all sakuyamon-zone)})}))
      (with-handlers ([exn:break? {λ [b] (and (newline) (values (shutdown #:kill? #true) "" (exn-message b)))}])
        (match ((curry sync/timeout/enable-break 1.618) (handle-evt (place-dead-evt sakuyamon) {λ _ 'dead-evt})
                                                        (handle-evt sakuyamon (curry cons sakuyamon)))
          [#false (values (shutdown #:kill? #true) "" "sakuyamon is delayed!")]
          ['dead-evt (values (shutdown) (port->string place-out) (port->string place-err))]
          [{list-no-order _ {? boolean? ssl?} {? number? port}} (values (shutdown) (sakuyamon-agent ssl? port "::1")
                                                                        (sakuyamon-agent ssl? port "127.0.0.1"))])))})

(define {sakuyamon-agent ssl? port host}
  {lambda arglist
    (define anyauth (make-parameter #false))
    (define location (make-parameter #false))
    (define user:pwd (make-parameter #false))
    (define go-headers (make-parameter null))
    (define http-method (make-parameter #"GET"))

    (define {retry uri . addition}
      (define {remake arglist}
        (remove* (list "--user" "-u" (user:pwd) uri) arglist))
      (apply (sakuyamon-agent ssl? port host) (append addition (remake arglist) (list uri))))
    
    (let/ec exit-agent
      ((curry parse-command-line "sakuyamon-curl" arglist)
       `{{usage-help ,(format "Transfer URL: a cURL-like tool for taming only.~n")}
         {once-each [{"--anyauth"} ,{λ [flag] (anyauth #true)} {"Detect authentication method."}]
                    [{"--location" "-L"} ,{λ [flag] (anyauth #true)} {"Follow redirects."}]
                    [{"--get" "-G"} ,{λ [flag] (http-method #"GET")} {"Send with HTTP GET."}]
                    [{"--head" "-I"} ,{λ [flag] (http-method #"HEAD")} {"Show document info only."}]
                    [{"--user" "-u"} ,{λ [flag u:p] (user:pwd u:p)} {"Server user and password." "USER:PASSWORD"}]
                    [{"--request" "-X"} ,{λ [flag method] (http-method (string->bytes/utf-8 (string-upcase method)))}
                                        {"Specify request command to use." "COMMAND"}]}
         {multi [{"--header" "-H"} ,{λ [flag line] (go-headers (cons line (go-headers)))}
                                   {"Custom header to pass to server." "LINE"}]}}
       {λ [! uri] (let ([status (let-values ([{status net-headers /dev/net/stdin} (http-sendrecv host uri #:ssl? ssl? #:port port
                                                                                                 #:method (http-method) #:headers (reverse (go-headers)))])
                                  (define parts (regexp-match #px".+?\\s+(\\d+)\\s+(.+)\\s*$" (bytes->string/utf-8 status)))
                                  (list (string->number (list-ref parts 1))
                                        (string-join (string-split (list-ref parts 2) (string cat#)) (string #\newline))
                                        (map {λ [kv] (cons (string->symbol (string-downcase (bytes->string/utf-8 (car kv))))
                                                           (bytes->string/utf-8 (cdr kv)))}
                                             (apply append (map extract-all-fields net-headers)))
                                        /dev/net/stdin))])
                    (match-define {list code _ headers _} status)
                    (with-handlers ([void {λ _ status}])
                      (cond [(and (location) (member code '{301 302 307}))
                             => {λ _ (case code
                                       [{301} (cond [(member (http-method) '{#"GET" #"HEAD"}) (retry (dict-ref headers 'location))]
                                                    [else status])]
                                       [{302 307} (retry (dict-ref headers 'location))])}]
                            [(and (user:pwd) (anyauth) (eq? code 401))
                             => {λ _ (let-values ([{WWW-Authenticate} (dict-ref headers 'www-authenticate)]
                                                  [{px.k=v} #px#"(\\w+)=\"(.+?)\""])
                                       (cond [(regexp-match #px"^Basic" WWW-Authenticate)
                                              => {λ _ (retry uri "--header" (format "Authorization: Basic ~a"
                                                                                    (base64-encode (string->bytes/utf-8 (user:pwd)))))}]
                                             [(regexp-match #px"^Digest" WWW-Authenticate)
                                              => {λ _ (with-handlers ([exn? {λ [e] (and (displayln e) (raise e))}])
                                                        (define bindings (for/list ([k-v (in-list (regexp-match* px.k=v WWW-Authenticate))])
                                                                           (let ([kv (regexp-match px.k=v k-v)])
                                                                             (cons (string->symbol (string-downcase (bytes->string/utf-8 (cadr kv))))
                                                                                   (caddr kv)))))
                                                        (match-define {list _ user pwd} (regexp-match #px"^([^:]+)?:(.+)?$" (user:pwd)))
                                                        (define nonce-count (~a "1" #:max-width 8 #:min-width 8 #:align 'right #:pad-string "0"))
                                                        (match-define {list realm qop nonce} (map (curry dict-ref bindings) '{realm qop nonce})) 
                                                        (define timestamp (number->string (current-seconds)))
                                                        (define cnonce (md5 (format "~a ~a" timestamp (md5 timestamp))))
                                                        (retry uri "--header" (format (string-append "Authorization: Digest nc=~a" 
                                                                                                     ", realm=\"~a\", username=\"~a\""
                                                                                                     ", nonce=\"~a\", cnonce=\"~a\""
                                                                                                     ", qop=\"~a\", uri=\"~a\""
                                                                                                     ", response=\"~a\"")
                                                                                      nonce-count realm user nonce cnonce qop uri
                                                                                      (md5 (format "~a:~a:~a:~a:~a:~a"
                                                                                                   ((password->digest-HA1 {λ _ pwd})
                                                                                                    user (bytes->string/utf-8 realm))
                                                                                                   nonce nonce-count cnonce qop
                                                                                                   (md5 (format "~a:~a" (http-method) uri)))))))}]
                                             [else (raise 500)]))}]
                            [else status])))}
       '{"url"}
       (compose1 exit-agent display (curryr string-replace #px"  -- : .+?-h --'\\s*" ""))))})
