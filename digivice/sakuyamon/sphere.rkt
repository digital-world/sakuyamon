#lang racket

(provide (all-defined-out))

(define desc "Update the flat authentication file to the digest one")

(module+ sakuyamon
  (require syntax/location)

  (require web-server/http/digest-auth)
  (require web-server/dispatchers/dispatch-passwords)
  
  (require "../../digitama/digicore.rkt")

  (define update-in-place? (make-parameter #false))
  
  (define update-realm.rktd
    (lambda [realm.rktd]
      (define {upgrade out}
        (define realms (cadr (with-input-from-file realm.rktd read)))
        (fprintf out "'{")
        (for ([item (in-list realms)]
              [group (in-naturals 1)])
          (match-define {list-rest realm pattern user.pwds} item)
          (define head (format "{~s " realm))
          (define indent (~a #:min-width (+ (string-length head) 2)))
          (fprintf out "~a~a~s~n" (if (eq? group 1) "" "  ") head pattern)
          (for ([user.pwd (in-list user.pwds)]
                [index (in-naturals 1)])
            (match-define {list user pwd} user.pwd)
            (define HA1 (cond [(eq? (string-length pwd) 32) (and 'already-done pwd)]
                              [else ((password->digest-HA1 {λ [user realm] pwd})
                                     (string-downcase (symbol->string user)) realm)]))
            (fprintf out "~a[~a \"~a\"]~a" indent user HA1
                     (if (eq? index (length user.pwds)) "}" #\newline)))
          (fprintf out "~a~n" (if (eq? group (length realms)) "}" ""))))
      (cond [(update-in-place?) (call-with-atomic-output-file realm.rktd {λ [out ftmp] (upgrade out)})]
            [else (upgrade (current-output-port))])))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            (parse-command-line (format "~a ~a" (cadr (quote-module-name)) (path-replace-suffix (file-name-from-path (quote-source-file)) #""))
                                (current-command-line-arguments)
                                `{{usage-help ,(format "~a~n" desc)}
                                  {once-each [{"--in-place"} ,{λ [flag] (update-in-place? #true)} {"Update file in place!"}]}}
                                {λ [! realm.rktd] (with-handlers ([void {λ _ (raise-user-error 'realm.rkt "Malformed data file: ~a" realm.rktd)}])
                                                    (define-values {auto authorize} (password-file->authorized? realm.rktd))
                                                    (authorize "just make sure" #"realm.rktd" #"well-formed")
                                                    (update-realm.rktd (simple-form-path realm.rktd)))}
                                '{"realm.rktd"}
                                (compose1 exit display (curryr string-replace #px"  -- : .+?-h --'\\s*" "")))))))
