#lang racket

(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

(define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
(define /stone/launchd.plist (build-path (digimon-stone) "launchd.plist"))

(define sakuyamon.smf "/lib/svc/manifest/network/sakuyamon.xml")
(define /stone/manifest.xml (build-path (digimon-stone) "manifest.xml"))

(define sakuyamon.service "/lib/systemd/system/sakuyamon.service")
(define /stone/systemd.service (build-path (digimon-stone) "systemd.service"))

(define sakuyamon.asl "/etc/asl/org.gyoudmon.sakuyamon")
(define /stone/sakuyamon.asl (build-path (digimon-stone) "asld.conf"))

(define sakuyamon.rsyslog "/etc/rsyslog.d/sakuyamon.conf")
(define /stone/sakuyamon.rsyslog (build-path (digimon-stone) "rsyslog.conf"))

(define sakuyamon.logadm "/etc/logadm.d/sakuyamon.conf")
(define /stone/sakuyamon.logadm (build-path (digimon-stone) "logadm.conf"))

(define targets
  (case (digimon-system)
    [{solaris} (list sakuyamon.smf sakuyamon.rsyslog sakuyamon.logadm)]
    [{macosx} (list sakuyamon.plist sakuyamon.asl)]
    [{linux} (list sakuyamon.service sakuyamon.rsyslog)]))

(module+ premake
  (when (string=? (current-tamer) "root")
    (or (system (format "sh ~a ~a create"
                        (build-path (digimon-stone) "tamer.sh")
                        (digimon-system)))
        (error 'make "Failed to separate privileges!"))))

(module+ make:files 
  (module+ make
    (define {sudo.make dest src [chown #false]}
      (and (with-output-to-file dest #:exists 'replace
             (thunk (and (putenv "destname" (path->string (file-name-from-path dest)))
                         (dynamic-require src #false))))
           (when chown (system (format "chown ~a ~a" chown dest)))))

    (when (string=? (current-tamer) "root")
      (make ([sakuyamon.plist [/stone/launchd.plist (quote-source-file)]
                              (sudo.make sakuyamon.plist /stone/launchd.plist "root:wheel")]
             [sakuyamon.smf [/stone/manifest.xml (quote-source-file)]
                            (sudo.make sakuyamon.smf /stone/manifest.xml "root:sys")]
             [sakuyamon.service [/stone/systemd.service (quote-source-file)]
                                (sudo.make sakuyamon.service /stone/systemd.service "root:root")]
             [sakuyamon.asl [/stone/sakuyamon.asl (quote-source-file)]
                            (and (sudo.make sakuyamon.asl /stone/sakuyamon.asl "root:wheel")
                                 (system "kill -s HUP `cat /var/run/syslog.pid`"))]
             [sakuyamon.rsyslog [/stone/sakuyamon.rsyslog (quote-source-file)]
                                (and (sudo.make sakuyamon.rsyslog /stone/sakuyamon.rsyslog "root:root")
                                     (or (system "svcadm restart system-log:rsyslog")
                                         (system "systemctl restart rsyslog")))]
             [sakuyamon.logadm [/stone/sakuyamon.logadm (quote-source-file)]
                               (sudo.make sakuyamon.logadm /stone/sakuyamon.logadm "root:sys")])
            targets)))

  (module+ clobber
    (when (string=? (current-tamer) "root")
      (system (format "sh ~a ~a delete"
                      (build-path (digimon-stone) "tamer.sh")
                      (digimon-system)))
      (for-each delete-file targets))))

(module+ postmake
  (when (string=? (current-tamer) "root")
    (system (format "sh ~a ~a ~a"
                    (build-path (digimon-stone) "realize.sh")
                    (digimon-system)
                    (car targets)))))
