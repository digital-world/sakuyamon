#lang typed/racket

(provide (all-defined-out))

(define desc : String "Update the flat authentication file to the digest one")

(module+ sakuyamon
  (require typed/web-server/http)
  
  (require "../../digitama/digicore.rkt")
  (require "../../digitama/unstable-typed/web-server/dispatchers.rkt")

  (require/typed racket
                 [call-with-atomic-output-file (-> Path-String (-> Output-Port Path-String Any) Any)])
  
  (define update-in-place? : (Parameterof Boolean) (make-parameter #false))
  
  (define update-realm.rktd : (-> Path-String Void)
    (lambda [realm.rktd]
      (define upgrade : (-> Output-Port Void)
        (lambda [out]
          (define /etc/passwd : (Listof HTTP-Password)
            (with-handlers ([exn? (λ [e] null)])
              (cadr (cast (with-input-from-file realm.rktd read) (List 'quote (Listof HTTP-Password))))))
          (fprintf out "'(")
          (cond [(null? /etc/passwd) (fprintf out ")~n")]
                [else (for ([item (in-list /etc/passwd)]
                            [group (in-naturals 1)])
                        (match-define (list-rest realm pattern user.pwds) item)
                        (define head (format "(~s " realm))
                        (define indent (~a #:min-width (+ (string-length head) 2 1)))
                        (fprintf out "~a~a~s~n" (if (eq? group 1) "" "  ") head pattern)
                        (for ([user.pwd (in-list user.pwds)]
                              [index (in-naturals 1)])
                          (match-define (list user pwd) user.pwd)
                          (define HA1 (cond [(eq? (string-length pwd) 32) (and 'already-done pwd)]
                                            [else ((password->digest-HA1 (λ [user realm] pwd))
                                                   (string-downcase (symbol->string user)) realm)]))
                          (fprintf out "~a[~a \"~a\"]~a" indent user HA1
                                   (if (eq? index (length user.pwds)) ")" #\newline)))
                        (fprintf out "~a~n" (if (eq? group (length /etc/passwd)) ")" "")))])))
      (cond [(false? (update-in-place?)) (upgrade (current-output-port))]
            [else (void (call-with-atomic-output-file realm.rktd (λ [[out : Output-Port] [ftmp : Path-String]] (upgrade out))))])))
  
  (call-as-normal-termination
   (thunk (parameterize ([current-directory (find-system-path 'orig-dir)])
            ((cast parse-command-line (-> String (Vectorof String) Help-Table (-> Any String Void) (Listof String) (-> String Void) Void))
             (format "~a ~a" (#%module) (path-replace-suffix (cast (file-name-from-path (#%file)) Path) #""))
             (current-command-line-arguments)
             `([usage-help ,(format "~a~n" desc)]
               [once-each [("--in-place") ,(λ [[_ : String]] (update-in-place? #true)) ("Update file in place!")]])
             (λ [! realm.rktd] (with-handlers ([void (λ _ (raise-user-error 'sphere "malformed data file: ~a" realm.rktd))])
                                 (define-values (<pwd-would-update-automatically> authorize) (password-file->authorized? realm.rktd))
                                 (authorize "ensure" #"realm.rktd" #"well-formed")
                                 (update-realm.rktd (simple-form-path realm.rktd))))
             '("realm.rktd")
             (lambda [[-h : String]]
               (display (string-replace -h #px"  -- : .+?-h --'\\s*" ""))
               (exit 0)))))))
