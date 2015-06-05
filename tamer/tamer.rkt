#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../digitama/digicore.rkt}
@require{../../DigiGnome/digitama/tamer.rkt}

(require file/md5)
(require net/head)
(require net/base64)
(require net/http-client)
(require web-server/http)

(require setup/dirs)
(require syntax/location)

(provide (all-defined-out))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))
(provide (all-from-out net/head net/base64 net/http-client web-server/http))

(define root? (string=? (current-tamer) "root"))
(define smf-daemon? (getenv "SMF_METHOD"))
(define realm.rktd (path->string (build-path (digimon-stone) "realm.rktd")))

(define /htdocs (curry format "/~a"))
(define /tamer (curry format "/~~~a/~a" (current-tamer)))
(define /digimon (curry format "/~~~a:~a/~a" (current-tamer) (current-digimon)))

(define ~htdocs (curry build-path (digimon-terminus)))
(define ~tamer (curry build-path (expand-user-path (format "~~~a" (current-tamer))) "DigitalWorld" "Kuzuhamon" "terminus"))
(define ~digimon (curry build-path (digimon-tamer) (car (use-compiled-file-paths)) "handbook"))

(define sakuyamon-agent
  (lambda [ssl? port host . arglist]
    (define anyauth (make-parameter #false))
    (define location (make-parameter #false))
    (define user:pwd (make-parameter #false))
    (define go-headers (make-parameter null))
    (define http-method (make-parameter #"GET"))
    
    (define {retry uri . addition}
      (define {remake arglist}
        (remove* (list "--user" "-u" (user:pwd) uri) arglist))
      (apply sakuyamon-agent ssl? port host (append addition (remake arglist) (list uri))))

    (define {on-recv status net-headers /dev/net/stdin}
      (define parts (regexp-match #px".+?\\s+(\\d+)\\s+(.+)\\s*$" (bytes->string/utf-8 status)))
      (list (string->number (list-ref parts 1))
            (string-join (string-split (list-ref parts 2) (string cat#)) (string #\newline))
            (map {λ [kv] (cons (string->symbol (string-downcase (bytes->string/utf-8 (car kv))))
                               (bytes->string/utf-8 (cdr kv)))}
                 (apply append (map extract-all-fields net-headers)))
            /dev/net/stdin))

    (define {authorize uri headers}
      (define WWW-Authenticate (dict-ref headers 'www-authenticate))
      (define px.k=v #px#"(\\w+)=\"(.+?)\"")
      (cond [(regexp-match #px"^Basic" WWW-Authenticate)
             => (thunk* (retry uri "--header" (format "Authorization: Basic ~a"
                                                      (base64-encode (string->bytes/utf-8 (user:pwd))))))]
            [(regexp-match #px"^Digest" WWW-Authenticate)
             => (thunk* (with-handlers ([exn? (λ [e] (and (displayln e) (raise e)))])
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
                                                                     ((password->digest-HA1 (thunk* pwd))
                                                                      user (bytes->string/utf-8 realm))
                                                                     nonce nonce-count cnonce qop
                                                                     (md5 (format "~a:~a" (http-method) uri))))))))]
            [else (raise 500)]))
    
    (let/ec exit-agent
      (command-line #:program "sakuyamon-curl"
                    #:argv arglist
                    #:usage-help "Transfer URL: a cURL-like tool for taming only." ""
                    #:once-each
                    [{"--anyauth"} "Detect authentication method."
                     (anyauth #true)]
                    [{"--location" "-L"} "Follow redirects."
                     (location #true)]
                    [{"--get" "-G"} "Send with HTTP GET."
                     (http-method #"GET")]
                    [{"--head" "-I"} "Show document info only."
                     (http-method #"HEAD")]
                    [{"--user" "-u"} USER:PASSWORD "Server user and password."
                     (user:pwd USER:PASSWORD)]
                    [{"--request" "-X"} COMMAND "Specify request command to use."
                     (http-method (string->bytes/utf-8 (string-upcase COMMAND)))]
                    #:multi
                    [{"--header" "-H"} LINE "Custom header to pass to server."
                     (go-headers (cons LINE (go-headers)))]
                    #:handlers
                    (λ [! uri] (let ([status ((curryr call-with-values on-recv)
                                              (thunk (http-sendrecv host uri #:ssl? ssl? #:port port
                                                                    #:method (http-method) #:headers (reverse (go-headers)))))])
                                 (match-define {list code _ headers _} status)
                                 (with-handlers ([void (const status)])
                                   (cond [(and (location) (member code '{301 302 307 308}) (member (http-method) '{#"GET" #"HEAD"}))
                                          => (thunk* (retry (dict-ref headers 'location)))]
                                         [(and (user:pwd) (anyauth) (eq? code 401))
                                          => (thunk* (authorize uri headers))]
                                         [else status]))))
                    '{"url"}
                    (compose1 exit-agent display (curryr string-replace #px"  -- : .+?-h --'\\s*" ""))))))


(define tamer-errmsg (make-parameter #false))
(define tamer-port (if root? 80 16180))
(define curl (curry sakuyamon-agent #false tamer-port "::1"))
(define 127.curl (curry sakuyamon-agent #false tamer-port "127.0.0.1"))

(define {check-ready? tips}
  (define {wrap-raise efne}
    (define errno (car (exn:fail:network:errno-errno efne)))
    (raise (cond [(not (and (eq? errno 146) (tamer-errmsg))) efne]
                 [else (struct-copy exn:fail:network:errno efne
                                    [message #:parent exn (tamer-errmsg)])])))
  (thunk (with-handlers ([exn:fail:network:errno? wrap-raise])
           (curl "-X" "Options" (~a "/" tips)))))

(parameterize ([current-custodian (make-custodian)]
               [current-subprocess-custodian-mode (if smf-daemon? #false 'interrupt)])
  (plumber-add-flush! (current-plumber) (λ [this] (custodian-shutdown-all (current-custodian))))
  ;;; These code will be evaluated in a flexibility way.
  ; * compile one file
  ; * compile multi files
  ; * run standalone
  ; * run as scribble
  ;;; In any situation, it will fork and only fork once.
  
  (define {try-fork efne}
    (define {raise-unless-ready efne}
      (define errno (car (exn:fail:network:errno-errno efne)))
      (unless (eq? errno 146) (raise efne)))

    (raise-unless-ready efne)
    (unless (find-executable-path "racket")
      (putenv "PATH" (format "~a:~a" (find-console-bin-dir) (getenv "PATH"))))
    (define-values {sakuyamon /dev/outin /dev/stdout /dev/errin}
      (subprocess #false #false #false
                  (format "~a/~a.rkt" (digimon-digivice) (current-digimon))
                  "realize" "-p" (number->string tamer-port)))

    (with-handlers ([exn:break? (compose1 (curry subprocess-kill sakuyamon) (const 'interrupt))])
      (unless (sync/enable-break /dev/outin (wrap-evt sakuyamon (const #false)))
        (tamer-errmsg (port->string /dev/errin))))

    ;;; has already forked as Solaris SMF daemon
    (when smf-daemon? (exit (subprocess-status sakuyamon))))

  ;;; to make the drracket background expansion happy
  (unless (regexp-match? #px#"drracket$" (find-system-path 'run-file))
    (when (or smf-daemon? (not root?)) ;;; test for the deployed one
      (with-handlers ([exn:fail:network:errno? try-fork])
        ((check-ready? (quote-source-file))))))
  (void))
