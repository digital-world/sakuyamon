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
      (define {curl ssl? port}
        {λ [uri #:host [host "::1"] #:method [method #"GET"] #:headers [headers null] #:data [data #false]]
          (define-values {status net-headers /dev/net/stdin}
            (http-sendrecv host uri #:ssl? ssl? #:port port #:method method #:headers headers #:data data))
          (define parts (regexp-match #px".+?\\s+(\\d+)\\s+(.+)\\s*$" (bytes->string/utf-8 status)))
          (list (string->number (list-ref parts 1))
                (string-join (string-split (list-ref parts 2) (string cat#)) (string #\newline))
                (map {λ [kv] (cons (string->symbol (string-downcase (bytes->string/utf-8 (car kv))))
                                   (bytes->string/utf-8 (cdr kv)))}
                     (apply append (map extract-all-fields net-headers)))
                /dev/net/stdin)})
      (with-handlers ([exn:break? {λ [b] (and (newline) (values (shutdown #:kill? #true) (cons "" (exn-message b))))}])
        (match ((curry sync/timeout/enable-break 1.618) (handle-evt (place-dead-evt sakuyamon) {λ _ 'dead-evt})
                                                        (handle-evt sakuyamon (curry cons sakuyamon)))
          [#false (values (shutdown #:kill? #true) (cons "" "sakuyamon is delayed!"))]
          ['dead-evt (values (shutdown) (cons (port->string place-out) (port->string place-err)))]
          [{list-no-order _ {? boolean? ssl?} {? number? port}} (values (shutdown) (curl ssl? port))]
          [whatever (values (shutdown #:kill? #true) (cons "" (format "~a" whatever)))])))})

(define sakuyamon-agent
  {lambda [curl uri0 method #:headers [headers0 null] #:data [data0 #false]]
    (define {retry [uri uri0] #:add-headers [headers null] #:data [data data0]}
      (sakuyamon-agent curl uri method #:headers (append headers0 headers) #:data data))
    
    (define status (curl uri0 #:method method #:headers headers0 #:data data0))
    (match-define {list status-code _ net-headers _} status)
    (with-handlers ([void {λ _ status}])
      (case status-code
        [{301} (let ([m (string->symbol (string-downcase (format "~a" method)))])
                 (cond [(member m '{get head}) (retry (dict-ref net-headers 'location))]
                       [else status]))]
        [{302 307} (retry (dict-ref net-headers 'location))]
        [{401} (let-values ([{username password} (values (string-downcase (read-line)) (read-line))]
                            [{WWW-Authenticate} (dict-ref net-headers 'www-authenticate)]
                            [{px.k=v} #px#"(\\w+)=\"(.+?)\""])
                 (cond [(regexp-match #px"^Basic" WWW-Authenticate)
                        => {λ _  (retry #:add-headers (list (bytes-append #"Authorization: Basic "
                                                                          (let ([user*pwd (format "~a:~a" username password)])
                                                                            (base64-encode (string->bytes/utf-8 user*pwd))))))}]
                       [(regexp-match #px"^Digest" WWW-Authenticate)
                        => {λ _ (with-handlers ([exn? {λ [e] (and (displayln e) (raise e))}])
                                  (define bindings (for/list ([k-v (in-list (regexp-match* px.k=v WWW-Authenticate))])
                                                     (let ([kv (regexp-match px.k=v k-v)])
                                                       (cons (string->symbol (string-downcase (bytes->string/utf-8 (cadr kv))))
                                                             (caddr kv)))))
                                  (define nonce-count #"00000001") ; needs 8 digits
                                  (match-define {list realm qop nonce} (map (curry dict-ref bindings) '{realm qop nonce}))
                                  (define private-key (string->bytes/utf-8 (symbol->string (gensym (current-digimon))))) 
                                  (define timestamp (string->bytes/utf-8 (number->string (current-seconds))))
                                  (define digest-uri (string->bytes/utf-8 uri0))
                                  (define cnonce (md5 (bytes-append timestamp #" " (md5 (bytes-append timestamp #":" private-key)))))
                                  (define RESPONSE (md5 (bytes-append ((password->digest-HA1 {λ [user realm] password})
                                                                       username (bytes->string/utf-8 realm)) #":"
                                                                      nonce #":" nonce-count #":" cnonce #":" qop #":"
                                                                      (md5 (bytes-append method #":" digest-uri)))))
                                  (retry #:add-headers (list (bytes-append #"Authorization: Digest nc=" nonce-count
                                                                           #", realm=\"" realm #"\""
                                                                           #", username=\"" (string->bytes/utf-8 username) #"\""
                                                                           #", nonce=\"" nonce #"\""
                                                                           #", cnonce=\"" cnonce #"\""
                                                                           #", qop=\"" qop #"\""
                                                                           #", uri=\"" digest-uri #"\""
                                                                           #", response=\"" RESPONSE #"\""))))}]
                       [else (raise 500)]))]
        [else status]))})
      