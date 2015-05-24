#lang racket

(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

(define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
(define /stone/launchd.plist (build-path (digimon-stone) "launchd.plist"))

(define sakuyamon.asl "/etc/asl/org.gyoudmon.sakuyamon")
(define /stone/sakuyamon.asl (build-path (digimon-stone) "asl.d.conf"))

(define sakuyamon.sh "/etc/init.d/sakuyamon")
(define /stone/initd.sh (build-path (digimon-stone) "initd.sh"))

{module+ make:files 
  {module+ make
    (define sudo.make {λ [dest src [chown #false]] (and (with-output-to-file dest #:exists 'replace
                                                          {λ _ (dynamic-require src #false)})
                                                        (when chown (system (format "chown ~a ~a" chown dest))))})
    
    (when (string=? (getenv "USER") "root")
      (or (system (format "sh ~a ~a create" (build-path (digimon-stone) "tamer.sh") (system-type 'os)))
          (error 'make "Failed to separate privileges!"))
    
      (make ([sakuyamon.plist [/stone/launchd.plist (quote-source-file)] (sudo.make sakuyamon.plist /stone/launchd.plist "root:wheel")]
             [sakuyamon.asl [/stone/sakuyamon.asl (quote-source-file)] (sudo.make sakuyamon.asl /stone/sakuyamon.asl "root:wheel")]
             [sakuyamon.sh [/stone/initd.sh (quote-source-file)] (sudo.make sakuyamon.sh /stone/initd.sh "root:root")])
            (case (system-type 'os)
              [{macosx} (list sakuyamon.plist sakuyamon.asl)]
              [{unix} (list sakuyamon.sh)])))}

  {module+ clobber
    (when (string=? (getenv "USER") "root")
      (system (format "sh ~a ~a delete" (build-path (digimon-stone) "tamer.sh") (system-type 'os)))
      (for-each delete-file (case (system-type 'os)
                              [{macosx} (list sakuyamon.plist sakuyamon.asl)]
                              [{unix} (list sakuyamon.sh)])))}}

{module+ postmake
  (when (string=? (getenv "USER") "root")
    (case (system-type 'os)
      [{macosx} (and (system (format "rm -fr ~a ~a" (build-path (digimon-stone) "stderr.log") (build-path (digimon-stone) "stdout.log")))
                     (system (format "launchctl unload ~a" sakuyamon.plist))
                     (system (format "launchctl load ~a" sakuyamon.plist)))]
      [{unix} (system (format "sh ~a restart" sakuyamon.sh))]))}
