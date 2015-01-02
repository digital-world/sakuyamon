#!/usr/bin/env racket
#lang racket

{module makefile racket
  (require make)
  (require setup/getinfo)
  
  (provide (all-defined-out))
  (provide (all-from-out make))
  
  (define make-dry-run (make-parameter #false))
  (define make-always-run (make-parameter #false))
  (define make-just-touch (make-parameter #false))
  (define make-no-submakes (make-parameter #false))
  (define current-real-targets (make-parameter null))
  (define current-make-goal (make-parameter #false))
  
  (make-print-dep-no-line #false)
  (make-print-checking #false)
  (make-print-reasons #false)
  
  (define rootdir (path-only (syntax-source #'rootdir))) ;;; Warning, this path is /-suffixed.
  (define dgvcdir (build-path rootdir "digivice"))
  (define dgtmdir (build-path rootdir "digitama"))
  (define vllgdir (build-path rootdir "village"))
  (define stnsdir (build-path rootdir "stone"))
  (define tmrsdir (build-path rootdir "tamer"))
  (define islndir (build-path rootdir "island"))
  
  (define info-ref (get-info/full rootdir))
  (define makefiles (filter file-exists?
                            (map (curryr build-path (file-name-from-path (syntax-source #'makefile)))
                                 (cons rootdir (with-handlers ([exn? (const null)])
                                                 (directory-list vllgdir #:build? #true))))))
  
  (define hack-rule
    {lambda [r]
      (define t (car r))
      (define ds (cadr r))
      (define f {lambda [] (with-handlers ([symbol? void])
                             (make-parent-directory* t)
                             (call-with-atomic-output-file t {lambda [whocares pseudo-t]
                                                               (close-output-port whocares)
                                                               ((caddr r) pseudo-t)
                                                               (when (make-dry-run)
                                                                 (raise 'make-dry-run #true))}))})
      (list (car r) (if (make-always-run) (cons rootdir ds) ds)
            (if (make-just-touch) {lambda [] (file-or-directory-modify-seconds t (current-seconds) f)} f))})}

{module make:check: racket
  (require (submod ".." makefile))
  
  (for ([handbook (in-list (map {lambda [type] (build-path tmrsdir type "handbook.scrbl")} (list "behavior")))]
        #:when (file-exists? handbook))
    (parameterize ([current-directory (path-only handbook)]
                   [current-namespace (make-base-namespace)])
        (namespace-require 'scribble/render)
        (eval '(require (prefix-in html: scribble/html-render)))
        (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                       #:render-mixin {lambda [%] (html:render-multi-mixin (html:render-mixin %))}
                       #:dest-dir ,(path-only handbook) #:quiet? #false))))}

{module+ main
  (require (submod ".." makefile))
  (require compiler/compiler)
  
  (define make:goal: {lambda [phony] (string->symbol (format "make:~a:" phony))})
  
  (define make-all
    {lambda []
      (compile-directory-zos rootdir info-ref #:verbose #true #:skip-doc-sources? #true)
      
      (let ([modpath `(submod ,(syntax-source #'makefile) make:files)])
        (when (module-declared? modpath)
          (dynamic-require modpath #false)
          (parameterize ([current-namespace (module->namespace modpath)])
            (file-or-directory-modify-seconds rootdir (current-seconds))
            (define rules (map hack-rule (foldr append null
                                                (filter {lambda [val]
                                                          (with-handlers ([exn? (const #false)])
                                                            (andmap {lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                     (procedure-arity-includes? (third ?) 1))} val))}
                                                        (filter-map {lambda [var] (namespace-variable-value var #false {lambda [] #false})}
                                                                    (namespace-mapped-symbols))))))
            (make/proc (cons (list (syntax-source #'I-am-here-just-for-fun) null void) rules)
                       (if (null? (current-real-targets)) (map car rules) (current-real-targets))))))
    
      (let ([modpath `(submod ,(syntax-source #'makefile) make:files make)])
        (when (module-declared? modpath)
          (dynamic-require modpath #false)))})
  
  (define make-clean
    {lambda []
      (define fclean {lambda [dirty]
                       (when (file-exists? dirty) (delete-file dirty))
                       (when (directory-exists? dirty) (delete-directory dirty))
                       (printf "make: deleted ~a.~n" (simplify-path dirty))})
      
      (let ([clbpath `(submod ,(syntax-source #'makefile) make:files clobber)])
        (when (and (member (current-make-goal) '{"distclean" "maintainer-clean"}) (module-declared? clbpath))
          (dynamic-require clbpath #false)))
      
      (let ([modpath `(submod ,(syntax-source #'makefile) make:files)])
        (when (module-declared? modpath)
          (dynamic-require modpath #false)
          (parameterize ([current-namespace (module->namespace modpath)])
            (define px.filter (pregexp (string-join #:before-first "^(.+?:)?" #:after-last ":.+:"
                                                     (member (string-replace (current-make-goal) #px"(?<!^)-?clean" "")
                                                             '{"maintainer" "dist" "clean" "mostly"}) "|")))
            (for ([var (in-list (namespace-mapped-symbols))]
                  #:when (regexp-match? px.filter (symbol->string var)))
              (for-each fclean (map {lambda [val] (if (list? val) (car val) val)}
                                    (namespace-variable-value var #false (const null))))))))
      
      (let ([px.exclude (pregexp (format "/(\\.git|~a)$" (path->string (file-name-from-path vllgdir))))]
            [px.include #px"/compiled/?"])
        (for-each fclean (reverse (filter (curry regexp-match? px.include)
                                          (sequence->list (in-directory rootdir (negate (curry regexp-match? px.exclude))))))))})
  
  (parse-command-line (file-name-from-path (syntax-source #'program))
                      (current-command-line-arguments)
                      `{{usage-help ,(format "Carefully our conventions are not exactly the same as those of GNU Make.~n")}
                        {once-each
                         [{"-B" "--always-make"}
                          ,{lambda [flag] (make-always-run #true)}
                          {"Unconditionally make all need-to-update targets."}]
                         [{"-n" "--test" "--dry-run"}
                          ,{lambda [flag] (make-dry-run #true)}
                          {"Do not actually update targets, just make. [Except Racket Sources]"}]
                         [{"-r" "--no-submakes"}
                          ,{lambda [flag] (make-no-submakes #true)}
                          {"Do not run submakes even if there is only phony targets."}]
                         [{"-s" "--silent" "--quiet"}
                          ,{lambda [flag] (current-output-port (open-output-nowhere '/dev/null #true))}
                          {"Just run commands but output nothing."}]
                         [{"-t" "--touch"}
                          ,{lambda [flag] (make-just-touch #true)}
                          {"Touch targets instead of remaking them if the target already exists."}]
                         [{"-v" "--verbose"}
                          ,{lambda [flag] (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true)}
                          {"Building with verbose messages."}]}}
                      {lambda [!voids . targets]
                        ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
                        ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
                        (use-compiled-file-paths (list (build-path "compiled")))
                        (define-values {files phonies} (partition filename-extension targets))
                        (parameterize ([current-real-targets (map {lambda [f] (if (relative-path? f) (build-path rootdir f) f)} files)])
                          (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                            (parameterize ([current-make-goal phony])
                              (cond [(string=? phony "all") (make-all)]
                                    [(regexp-match? #px"clean$" phony) (make-clean)]
                                    [else (let ([modpath `(submod ,(syntax-source #'makefile) ,(make:goal: phony))])
                                            (if (module-declared? modpath)
                                                (dynamic-require modpath #false)
                                                (eprintf "make: I don't know how to make `~a`!~n" phony)))]))))
                        (when (and (null? files) (not (make-no-submakes)))
                          (for ([submake (in-list (cdr makefiles))])
                            (printf "make: submake: ~a~n" submake)
                            (define submain `(submod ,submake main))
                            (dynamic-require (if (module-declared? submain #true) submain submake) #false)
                            (printf "make: submade: ~a~n" submake)))}
                      (list "phony-target|file-path")
                      {lambda [--help]
                        (display (foldl {lambda [-h --help] (if (string? -h) (string-append --help -h) --help)}
                                        (string-replace --help #px"  -- : .+?-h --'."
                                                        (string-join #:before-first (format "~n where <phony-target> is one of~n  ") #:after-last (format "~n")
                                                                     '{"all : Building the entire software with generating documentation. [default]"
                                                                       "mostlyclean : Delete all files except that people normally don't want to reconstruct."
                                                                       "clean : Delete all files except that records the configuration."
                                                                       "distclean : Delete all files that are not included in the distribution."
                                                                       "maintainer-clean : Delete all files that can be reconstructed. [Maintainers Only]"}
                                                                     (format "~n  ")))
                                        (map {lambda [phony] (let ([sub `(submod ,(syntax-source #'makefile) ,(make:goal: (car phony)))])
                                                               (when (module-declared? sub) (format "  ~a : ~a~n" (car phony) (cdr phony))))}
                                             (list (cons 'install "Installing the software, then running test if testcases exist.")
                                                   (cons 'uninstall "Delete all the installed files and documentation.")
                                                   (cons 'dist "Creating a distribution file of the source files.")
                                                   (cons 'check "Performing self tests on the program this makefile builds before building.")
                                                   (cons 'installcheck "Performing installation tests on the target system after installing.")))))
                        (exit 0)}
                      {lambda [unknown]
                        (eprintf "make: I don't know what does `~a` mean!~n" unknown)
                        (exit 1)})}
