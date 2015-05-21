#lang racket

(require "digitama/posix.rkt")
(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

{module+ make:files
  (define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
  (define sakuyamon.sh "/etc/init.d/sakuyamon")

  (define /stone/launchd.plist (build-path (digimon-stone) "launchd.plist"))
  (define /stone/initd.sh (build-path (digimon-stone) "initd.sh"))
  
  {module+ make
    (define sudo.make {λ [dest src] (with-output-to-file dest #:exists 'replace
                                      {λ _ (dynamic-require src #false)})})
    
    (when (zero? (getuid))
      (or (system "sh ~a ~a create" (build-path (digimon-stone) "tamer.sh") (system-type 'os))
          (error 'make "Failed to separate privileges!")))

    (make ([sakuyamon.plist [/stone/launchd.plist (quote-source-file)] (void (sudo.make sakuyamon.plist /stone/launchd.plist)
                                                                             (system (format "chown root:wheel ~a" sakuyamon.plist)))]
           [sakuyamon.sh [/stone/initd.sh (quote-source-file)] (void (sudo.make sakuyamon.sh /stone/initd.sh)
                                                                     (system (format "chown root:root ~a" sakuyamon.sh)))])
          (list (case (system-type 'os)
                  [{macosx} sakuyamon.plist]
                  [{unix} sakuyamon.sh])))}
  
  {module+ clobber
    (when (zero? (getuid))
      (system "sh ~a ~a delete" (build-path (digimon-stone) "tamer.sh") (system-type 'os))
      (delete-file (case (system-type 'os)
                     [{macosx} sakuyamon.plist]
                     [{unix} sakuyamon.sh])))}}
