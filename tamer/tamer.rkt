#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../digitama/digicore.rkt}
@require{../../DigiGnome/digitama/tamer.rkt}

(require web-server/test)
(require net/http-client)

(provide (all-defined-out))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))
(provide (all-from-out net/http-client web-server/test))

(define sakuyamon-realize
  {lambda arglist
    (define-values {sakuyamon place-in place-out place-err}
      {place* #:in #false #:out #false #:err #false ping
              (parameterize ([current-command-line-arguments (place-channel-get ping)]
                             [|tamer:use at your risk| ping])
                (dynamic-require `(submod ,(build-path (digimon-digivice) "sakuyamon/realize.rkt") sakuyamon) #false))})
    (place-channel-put sakuyamon (list->vector (if (member "-p" arglist) arglist (list* "-p" "0" arglist))))
    (define {shutdown #:kill? [kill? #false]}
      {λ _ (and (close-output-port place-in)
                (for-each close-input-port (list place-out place-err))
                (when kill? (place-kill sakuyamon))
                (place-wait sakuyamon))})
    (with-handlers ([exn:break? {λ [b] (and (newline) (values (shutdown (exn-message b)) #false))}])
      (match (sync/timeout/enable-break 1.618 (handle-evt (place-dead-evt sakuyamon) {λ _ 'dead-evt}) (handle-evt sakuyamon (curry cons sakuyamon)))
        [{? false?} (values (shutdown #:kill? #true) (cons "" "sakuyamon is delayed!"))]
        ['dead-evt (values (shutdown) (cons (port->string place-out) (port->string place-err)))]
        [{list-no-order _ {? boolean? ssl?} {? number? port}} (values (shutdown) {λ [url] (http-sendrecv "127.0.0.1" url #:ssl? ssl? #:port port)})]))})
