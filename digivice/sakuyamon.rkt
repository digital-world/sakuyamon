#lang typed/racket

(require "../../DigiGnome/digitama/runtime.typed.rkt")

(current-digimon "sakuyamon")
(current-directory (digimon-digivice))

(define show-help-and-exit : {[#:exitcode Integer] [#:errcmd (Option String)] -> Void}
  {lambda [#:exitcode [exitcode 0] #:errcmd [error-command #false]]
    (define printf0 : {String Any * -> Void} (if error-command eprintf printf))
    (define cmds : (Listof String) (#{filter-map @ String Path}
                                    {λ [cmd] (and (regexp-match? #px"\\.rkt$" cmd) (path->string cmd))}
                                    (directory-list "sakuyamon")))
    (define width : Natural (string-length (argmax string-length cmds)))
    (printf0 "Usage: sakuyamon <command> [<option> ...] [<arg> ...]~n~nAll available commands:~n")
    (for ([cmd : String (in-list cmds)])
      (printf0 "  ~a ~a~n" (~a (regexp-replace #px"^(.+).rkt$" cmd "\\1") #:min-width width)
               (car (#{call-with-values @ (Listof Any)}
                     {λ _ (dynamic-require `(file ,(format "sakuyamon/~a" cmd)) 'desc {λ _ "[Mission Description]"})}
                     list))))
    (when (string? error-command)
      (newline)
      (raise-user-error 'sakuyamon "Unrecognized command: ~a" error-command))})

(define main : Racket-Main
  {match-lambda*
    [{? null? _} (show-help-and-exit)]
    [{cons "help" _} (show-help-and-exit)]
    [{cons cmd argv} (parameterize ([current-command-line-arguments (list->vector argv)]
                                    [current-namespace (make-base-namespace)])
                       (define cmd.rkt : Path-String (format "sakuyamon/~a.rkt" cmd))
                       (if (file-exists? cmd.rkt)
                           (call-with-values {λ _ (eval `(require (submod (file ,cmd.rkt) sakuyamon)))} void)
                           (show-help-and-exit #:exitcode 1 #:errcmd cmd)))]})

;;; `raco setup` makes it hard to set --main option when making launcher
(exit-with-fixed-code (apply main (vector->list (current-command-line-arguments))))
