#lang racket

(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

(define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
(define sakuyamon.sh "/etc/init.d/sakuyamon")

(define /stone/launchd.plist (build-path (digimon-stone) "launchd.plist"))
(define /stone/initd.sh (build-path (digimon-stone) "initd.sh"))

{module+ premake
  (define sudo.make {λ [dest src] (with-output-to-file dest #:exists 'replace
                                    {λ _ (dynamic-require src #false)})})
  
  (when (string=? (getenv "USER") "root")
    (or (system (format "sh ~a ~a create" (build-path (digimon-stone) "tamer.sh") (system-type 'os)))
        (error 'make "Failed to separate privileges!"))
    
    (make ([sakuyamon.plist [/stone/launchd.plist (quote-source-file)] (void (sudo.make sakuyamon.plist /stone/launchd.plist)
                                                                             (system (format "chown root:wheel ~a" sakuyamon.plist)))]
           [sakuyamon.sh [/stone/initd.sh (quote-source-file)] (void (sudo.make sakuyamon.sh /stone/initd.sh)
                                                                     (system (format "chown root:root ~a" sakuyamon.sh)))])
          (list (case (system-type 'os)
                  [{macosx} sakuyamon.plist]
                  [{unix} sakuyamon.sh]))))}

{module+ make:files 
  {module+ clobber
    (when (string=? (getenv "USER") "root")
      (system (format "sh ~a ~a delete" (build-path (digimon-stone) "tamer.sh") (system-type 'os)))
      (delete-file (case (system-type 'os)
                     [{macosx} sakuyamon.plist]
                     [{unix} sakuyamon.sh])))}}

{module+ postmake
  (when (string=? (getenv "USER") "root")
    (case (system-type 'os)
      [{macosx} (and (system (format "rm -fr ~a ~a" (build-path (digimon-stone) "stderr.log") (build-path (digimon-stone) "stdout.log")))
                     (system (format "launchctl unload ~a" sakuyamon.plist))
                     (system (format "launchctl load ~a" sakuyamon.plist)))]
      [{unix} (system (format "sh ~a restart" sakuyamon.sh))]))}
