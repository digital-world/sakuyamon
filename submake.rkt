#lang racket

(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

{module+ make:files
  (define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
  (define sakuyamon.sh "/etc/init.d/sakuyamon")

  (define /stone/launchd.plist (build-path (digimon-stone) "launchd.plist"))
  (define /stone/initd.sh (build-path (digimon-stone) "initd.sh"))

  (define sudo.make {λ [dest src] (let ([daemon (make-temporary-file)])
                                    (with-output-to-file daemon #:exists 'replace
                                      {λ _ (dynamic-require src #false)})
                                    (system (format "sudo mv ~a ~a" daemon dest)))})
  (define sudo.clobber {λ [dest] (system (format "sudo rm ~a" dest))})
  
  {module+ make
    (define daemon (case (system-type 'os)
                     [{macosx} (parameterize ([current-output-port /dev/null])
                                 (or (system "dseditgroup -o read tamer")
                                     (system "sudo dseditgroup -o create tamer")
                                     (error 'dscl "Failed to create group: ~a." 'tamer))
                                 (or (system (format "dseditgroup -o checkmember -m ~a tamer" (getenv "USER")))
                                     (system (format "sudo dseditgroup -o edit -a ~a -t user tamer" (getenv "USER")))
                                     (error 'dscl "Failed to add ~a to group ~a." (getenv "USER") 'tamer))
                                 sakuyamon.plist)]
                     [{unix} (parameterize ([current-output-port /dev/null])
                               (or (system "getent group tamer")
                                   (system "sudo groupadd tamer")
                                   (error 'dscl "Failed to create group: ~a." 'tamer))
                               (or (system "id -Gn | grep '\btamer\b")
                                   (system (format "sudo usermod -a -G tamer ~a" (getenv "USER")))
                                   (error 'dscl "Failed to add ~a to group ~a." (getenv "USER") 'tamer))
                               sakuyamon.sh)]))

    (make ([sakuyamon.plist [/stone/launchd.plist (quote-source-file)] (void (sudo.make sakuyamon.plist /stone/launchd.plist)
                                                                             (system (format "sudo chown root:wheel ~a" sakuyamon.plist)))]
           [sakuyamon.sh [/stone/initd.sh (quote-source-file)] (void (sudo.make sakuyamon.sh /stone/initd.sh)
                                                                     (system (format "sudo chown root:root ~a" sakuyamon.sh)))])
          (list daemon))}
  
  {module+ clobber
    (case (system-type 'os)
      [{macosx} (void (system (format "sudo dseditgroup -o edit -d ~a -t user tamer" (getenv "USER")))
                      (sudo.clobber sakuyamon.plist))]
      [{unix} (sudo.clobber sakuyamon.sh)])}}
