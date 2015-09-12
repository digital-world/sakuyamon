#lang at-exp racket

;;; TODO: This module should be implemented in Typed Racket
;;; but meanwhile structs in this module has lots of uncovertable contracts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reference: RFC5424                                                                          ;;;
;;; SYSLOG-MSG      = HEADER SP STRUCTURED-DATA [SP MSG]                                        ;;;
;;; HEADER          = PRI VERSION SP TIMESTAMP SP HOSTNAME SP APP-NAME SP PROCID SP MSGID       ;;;
;;; PRI             = "<" PRIVAL ">"                                                            ;;;
;;; PRIVAL          = 1*3DIGIT ; range 0 .. 191                                                 ;;;
;;; VERSION         = NONZERO-DIGIT 0*2DIGIT                                                    ;;;
;;; HOSTNAME        = NILVALUE / 1*255PRINTUSASCII                                              ;;;
;;; APP-NAME        = NILVALUE / 1*48PRINTUSASCII                                               ;;;
;;; PROCID          = NILVALUE / 1*128PRINTUSASCII                                              ;;;
;;; MSGID           = NILVALUE / 1*32PRINTUSASCII                                               ;;;
;;; TIMESTAMP       = NILVALUE / FULL-DATE "T" FULL-TIME                                        ;;;
;;; FULL-DATE       = DATE-FULLYEAR "-" DATE-MONTH "-" DATE-MDAY                                ;;;
;;; DATE-FULLYEAR   = 4DIGIT                                                                    ;;;
;;; DATE-MONTH      = 2DIGIT  ; 01-12                                                           ;;;
;;; DATE-MDAY       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on month/year                  ;;;
;;; FULL-TIME       = PARTIAL-TIME TIME-OFFSET                                                  ;;;
;;; PARTIAL-TIME    = TIME-HOUR ":" TIME-MINUTE ":" TIME-SECOND [TIME-SECFRAC]                  ;;;
;;; TIME-HOUR       = 2DIGIT  ; 00-23                                                           ;;;
;;; TIME-MINUTE     = 2DIGIT  ; 00-59                                                           ;;;
;;; TIME-SECOND     = 2DIGIT  ; 00-59                                                           ;;;
;;; TIME-SECFRAC    = "." 1*6DIGIT                                                              ;;;
;;; TIME-OFFSET     = "Z" / TIME-NUMOFFSET                                                      ;;;
;;; TIME-NUMOFFSET  = ("+" / "-") TIME-HOUR ":" TIME-MINUTE                                     ;;;
;;;                                                                                             ;;;
;;; STRUCTURED-DATA = NILVALUE / 1*SD-ELEMENT                                                   ;;;
;;; SD-ELEMENT      = "[" SD-ID *(SP SD-PARAM) "]"                                              ;;;
;;; SD-PARAM        = PARAM-NAME "=" %d34 PARAM-VALUE %d34                                      ;;;
;;; SD-ID           = SD-NAME                                                                   ;;;
;;; PARAM-NAME      = SD-NAME                                                                   ;;;
;;; PARAM-VALUE     = UTF-8-STRING ; characters '"', '\' and ']' MUST be escaped.               ;;;
;;; SD-NAME         = 1*32PRINTUSASCII ; except '=', SP, ']', %d34 (")                          ;;;
;;; MSG             = MSG-ANY / MSG-UTF8                                                        ;;;
;;; MSG-ANY         = *OCTET ; not starting with BOM                                            ;;;
;;; MSG-UTF8        = BOM UTF-8-STRING                                                          ;;;
;;; BOM             = %xEF.BB.BF                                                                ;;;
;;; UTF-8-STRING    = *OCTET ; UTF-8 string as specified in RFC 3629                            ;;;
;;; OCTET           = %d00-255                                                                  ;;;
;;; SP              = %d32                                                                      ;;;
;;; PRINTUSASCII    = %d33-126                                                                  ;;;
;;; NONZERO-DIGIT   = %d49-57                                                                   ;;;
;;; DIGIT           = %d48 / NONZERO-DIGIT                                                      ;;;
;;; NILVALUE        = "-"                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide (struct-out Syslog-Message))
(provide (struct-out log:request))

(require "posix.rkt")
(require "geolocation.rkt")

(struct syslog (facility severity timestamp loghost sender #| also known as TAG |# pid message) #:prefab)

(define string->syslog
  (lambda [log-text]
    (define template (pregexp (format "^<(~a)>\\s*(~a)\\s+(~a)\\s+(~a)\\[(~a)\\]:?\\s*(~a)?\\s*(~a)?$"
                                      "\\d{1,3}" #| prival |#
                                      "[^:]+[^ ]+" #| timestamp |#
                                      "[^ ]+" #| hostname |#
                                      "[^[]+" #| appname |#
                                      "\\d+"  #| procid |#
                                      "\\[[^]]+\\]" #| structured data, just ignored |#
                                      ".+" #| free-from message |#)))
    (define (~geolocation ip . whocares)
      (define geo (what-is-my-address ip))
      (cond [(false? geo) ip]
            [(false? (geolocation-city geo)) (format "~a[~a/~a]" ip (geolocation-continent geo) (geolocation-country geo))]
            [else (format "~a[~a ~a]" ip (geolocation-country geo) (geolocation-city geo))]))
    (match (regexp-match template log-text)
      [(? false?) (error 'string->syslog "Invalid Syslog Message: ~a" log-text)]
      [(list _ prival timestamp hostname appname procid _ ffmsg)
       (let-values ([[facility severity] (quotient/remainder (string->number prival) 8)])
         (syslog ((ctype-c->scheme _facility) (arithmetic-shift facility 3))
                 ((ctype-c->scheme _severity) severity)
                 timestamp
                 hostname
                 appname
                 (string->number (format "~a" procid))
                 (match ffmsg
                   [(? false?) #false]
                   [(pregexp #px"\\s*request:\\s*(.+)" (list _ hstr)) (string->request hstr)]
                   [else (regexp-replace* #px"\\d{1,3}(\\.\\d{1,3}){3}" ffmsg ~geolocation)])))])))

(module* typed typed/racket
  (provide (all-defined-out))
  
  (define-type Syslog syslog)

  (require/typed/provide (submod "..")
                         [#:struct Syslog-Message ()]
                         [#:struct (log:request Syslog-Message)
                          ([timestamp : (Option String)]
                           [method : (Option String)]
                           [uri : (Option String)]
                           [client : (Option String)]
                           [host : (Option String)]
                           [user-agent : (Option String)]
                           [referer : (Option String)]
                           [headers : (HashTable Symbol (U String Bytes))])]
                         [#:struct syslog
                          ([facility : Symbol]
                           [severity : Symbol]
                           [timestamp : String]
                           [loghost : String]
                           [sender : String]
                           [pid : (Option Index)]
                           [message : (U False String Syslog-Message)])]
                         [string->syslog (-> String Syslog)]))

(module digitama racket
  (provide (all-defined-out))

  (struct Syslog-Message () #:prefab)
  (struct log:request Syslog-Message (timestamp method uri client host user-agent referer headers) #:prefab)

  (define (string->request hstr)
    (define headers (for/hash ([(key val) (in-hash (read (open-input-string hstr)))])
                      (values key (if (bytes? val) (bytes->string/utf-8 val) val))))
    (log:request (hash-ref headers 'logging-timestamp (const #false))
                 (hash-ref headers 'method (const #false))
                 (hash-ref headers 'uri (const #false))
                 (hash-ref headers 'client (const #false))
                 (hash-ref headers 'host (const #false))
                 (hash-ref headers 'user-agent (const #false))
                 (hash-ref headers 'referer (const #false))
                 headers)))

(require (submod "." digitama))
