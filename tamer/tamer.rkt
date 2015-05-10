#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../digitama/digicore.rkt}
@require{../../DigiGnome/digitama/tamer.rkt}

(require (submod "../digivice/sakuyamon/realize.rkt" digitama))

(require net/head)
(require net/base64)
(require net/http-client)
(require web-server/http)
(require web-server/test)

(provide (all-defined-out))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))
(provide (all-from-out net/head net/base64 net/http-client web-server/http web-server/test))

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
      (define {sendrecv ssl? port}
        {λ [uri #:host [host "::1"] #:method [method #"GET"] #:headers [headers null] #:data [data #false]]
          (define-values {status net-headers /dev/net/stdin}
            (http-sendrecv host uri #:ssl? ssl? #:port port #:method method #:headers headers #:data data))
          (define parts (regexp-match #px".+?\\s+(\\d+)\\s+(.+)\\s*$" (bytes->string/utf-8 status)))
          (list (string->number (list-ref parts 1))
                (string-join (string-split (list-ref parts 2) (string cat#)) (string #\newline))
                (apply append (map extract-all-fields net-headers))
                /dev/net/stdin)})
      (with-handlers ([exn:break? {λ [b] (and (newline) (values (shutdown #:kill? #true) (cons "" (exn-message b))))}])
        (match ((curry sync/timeout/enable-break 1.618) (handle-evt (place-dead-evt sakuyamon) {λ _ 'dead-evt})
                                                        (handle-evt sakuyamon (curry cons sakuyamon)))
          [#false (values (shutdown #:kill? #true) (cons "" "sakuyamon is delayed!"))]
          ['dead-evt (values (shutdown) (cons (port->string place-out) (port->string place-err)))]
          [{list-no-order _ {? boolean? ssl?} {? number? port}} (values (shutdown) (sendrecv ssl? port))]
          [whatever (values (shutdown #:kill? #true) (cons "" (format "~a" whatever)))])))})

(define sakuyamon-agent
  {lambda [sendrecv uri0 method #:headers [headers0 null] #:data [data0 #false]　#:username [user0 #false] #:password [pwd0 #false]]
    (define {re-sendrecv [uri uri0] #:add-headers [headers null] #:data [data data0] #:username [user user0] #:password [pwd pwd0]}
      (sakuyamon-agent sendrecv uri method #:headers (append headers0 headers) #:data data #:username user #:password pwd))
    
    (define status (sendrecv uri0 #:method method #:headers headers0 #:data data0))
    (match-define {list status-code _ net-headers _} status)
    (case status-code
      [{301} (let ([m (string->symbol (string-downcase (format "~a" method)))])
               (cond [(member m '{get head}) (re-sendrecv (dict-ref net-headers #"Location"))]
                     [else status]))]
      [{302 307} (re-sendrecv (dict-ref net-headers #"Location"))]
      [{401} (with-handlers ([exn? {λ _ status}])
               (re-sendrecv #:username #false #:password #false
                            #:add-headers (list (bytes-append #"Authorization: Basic "
                                                              (base64-encode (bytes-append user0 #":" pwd0))))))]
      [else status])})
      