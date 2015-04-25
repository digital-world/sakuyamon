#lang racket/base

(provide (all-defined-out))

(define desc "Launch the Web Server with Listening All Addresses")

{module+ sakuyamon
  (require racket/unit)
  (require racket/string)
  (require racket/cmdline)
  (require racket/promise)
  (require racket/function)
  (require racket/place)
  (require racket/async-channel)
  
  (require net/tcp-sig)
  (require web-server/private/dispatch-server-sig)
  (require web-server/private/dispatch-server-unit)

  (require "../../digitama/digicore.rkt")
  (require "../../digitama/configuration.rkt")

  (define {serve-forever}
    (define ping (make-async-channel #false))
    (define-compound-unit sakuyamon@
      (import)
      (export DS)
      (link [{{DS : dispatch-server^}} dispatch-server@ T DSC]
            [{{T : tcp^}} (force sakuyamon-tcp@) DS]
            [{{DSC : dispatch-server-config^}} sakuyamon-config@ DS]))
    
    (define-values/invoke-unit/infer sakuyamon@)
    
    (define shutdown (serve #:confirmation-channel ping))
    (define pinged (async-channel-get ping))
    (dynamic-wind void
                  {λ _ (cond [(and (exn:fail:network:errno? pinged) pinged)
                              => (compose1 exit {λ _ (car (exn:fail:network:errno-errno pinged))} (curry eprintf "~a~n") exn-message)] 
                             [else (with-handlers ([exn:break? {λ _ (unless (port-closed? (current-output-port)) (newline))}])
                                     (printf "sakuyamon@HTTP~a#~a~n" (if (sakuyamon-ssl?) "S" "") pinged)
                                     (when (place-channel? (|tamer:use at your risk|))
                                       (place-channel-put (|tamer:use at your risk|) (list (sakuyamon-ssl?) pinged)))
                                     (let do-not-return ([stdin (current-input-port)])
                                       (unless (eof-object? (read-line stdin))
                                         (sync/enable-break (handle-evt stdin do-not-return)))))])}
                  shutdown))

  (call-as-normal-termination
   {λ _ (parse-command-line "sakuyamon realize"
                            (current-command-line-arguments)
                            `{{usage-help ,(format "~a~n" desc)}
                              {once-each [{"-p"} ,{λ [flag port] (sakuyamon-port (string->number port))}
                                                 {"Use an alternative <port>." "port"}]
                                         [{"-w"} ,{λ [flag mw] (sakuyamon-max-waiting (string->number mw))}
                                                 {"Maximum number of clients can be waiting for acceptance." "mw"}]
                                         [{"-t"} ,{λ [flag ict] (sakuyamon-connection-timeout (string->number ict))}
                                                 {"Initial connection timeout." "ict"}]
                                         [{"--SSL"} ,{λ [flag] (sakuyamon-ssl? #true)} {"Enable SSL with 443 as default port."}]}}
                            {λ [! . arglist] (if (null? arglist) (serve-forever) (raise-user-error 'sakuyamon "I don't need arguments: ~a" arglist))}
                            null
                            {λ [--help] (exit (display (string-replace --help #px"  -- : .+?-h --'\\s*" "")))})})}
