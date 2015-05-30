#lang racket

(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

(define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
(define /stone/launchd.plist (build-path (digimon-stone) "launchd.plist"))

(define sakuyamon.sh "/etc/init.d/sakuyamon")
(define /stone/initd.sh (build-path (digimon-stone) "initd.sh"))

(define sakuyamon.asl "/etc/asl/org.gyoudmon.sakuyamon")
(define /stone/sakuyamon.asl (build-path (digimon-stone) "asld.conf"))

(define sakuyamon.rsyslog "/etc/asl/org.gyoudmon.sakuyamon")
(define /stone/sakuyamon.rsyslog (build-path (digimon-stone) "rsyslog.conf"))

{module+ make:files 
  {module+ make
    (define sudo.make {λ [dest src [chown #false]] (and (with-output-to-file dest #:exists 'replace
                                                          {λ _ (and (putenv "destname" (path->string (file-name-from-path dest)))
                                                                    (dynamic-require src #false))})
                                                        (when chown (system (format "chown ~a ~a" chown dest))))})
    
    (when (string=? (getenv "USER") "root")
      (or (system (format "sh ~a ~a create" (build-path (digimon-stone) "tamer.sh") (system-type 'os)))
          (error 'make "Failed to separate privileges!"))
    
      (make ([sakuyamon.plist [/stone/launchd.plist (quote-source-file)] (sudo.make sakuyamon.plist /stone/launchd.plist "root:wheel")]
             [sakuyamon.sh [/stone/initd.sh (quote-source-file)] (and (sudo.make sakuyamon.sh /stone/initd.sh "root:root")
                                                                      (system (format "chmod a+x ~a" sakuyamon.sh)))]
             [sakuyamon.asl [/stone/sakuyamon.asl (quote-source-file)] (and (sudo.make sakuyamon.asl /stone/sakuyamon.asl "root:wheel")
                                                                            (system "kill -s HUP `cat /var/run/syslog.pid`"))]
             [sakuyamon.rsyslog [/stone/sakuyamon.rsyslog (quote-source-file)] (and (sudo.make sakuyamon.rsyslog /stone/sakuyamon.rsyslog "root:root")
                                                                                    (system "kill -s HUP `cat /var/run/rsyslog.pid`"))])
            (case (system-type 'os)
              [{macosx} (list sakuyamon.plist sakuyamon.asl)]
              [{unix} (list sakuyamon.sh)])))}

  {module+ clobber
    (when (string=? (getenv "USER") "root")
      (system (format "sh ~a ~a delete" (build-path (digimon-stone) "tamer.sh") (system-type 'os)))
      (for-each delete-file (case (system-type 'os)
                              [{macosx} (list sakuyamon.plist sakuyamon.asl)]
                              [{unix} (list sakuyamon.sh sakuyamon.rsyslog)])))}}

{module+ postmake
  (when (string=? (getenv "USER") "root")
    (case (system-type 'os)
      [{macosx} (or (system (format "pkill -1 -u `id -u tamer`"))
                    (system (format "launchctl load ~a" sakuyamon.plist)))]
      [{unix} (or (system (format "service sakuyamon reload"))
                  (system (format "service sakuyamon start")))]))}
