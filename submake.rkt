#lang racket

(require "digitama/digicore.rkt")

(require syntax/location)
(require make)

(define sakuyamon.plist "/System/Library/LaunchDaemons/org.gyoudmon.sakuyamon.plist")
(define /sakuyamon/launchd.plist (build-path (digimon-stone) "sakuyamon" "launchd.plist"))
(define foxpipe.plist "/System/Library/LaunchDaemons/org.gyoudmon.foxpipe.plist")
(define /foxpipe/launchd.plist (build-path (digimon-stone) "foxpipe" "launchd.plist"))

(define sakuyamon.smf "/lib/svc/manifest/network/sakuyamon.xml")
(define /sakuyamon/manifest.xml (build-path (digimon-stone) "sakuyamon" "manifest.xml"))
(define foxpipe.smf "/lib/svc/manifest/network/foxpipe.xml")
(define /foxpipe/manifest.xml (build-path (digimon-stone) "foxpipe" "manifest.xml"))

(define sakuyamon.service "/lib/systemd/system/sakuyamon.service")
(define /sakuyamon/systemd.service (build-path (digimon-stone) "sakuyamon" "systemd.service"))
(define foxpipe.service "/lib/systemd/system/foxpipe.service")
(define /foxpipe/systemd.service (build-path (digimon-stone) "foxpipe" "systemd.service"))

(define sakuyamon.asl "/etc/asl/org.gyoudmon.sakuyamon")
(define /stone/sakuyamon.asl (build-path (digimon-stone) "asld.conf"))

(define sakuyamon.rsyslog "/etc/rsyslog.d/sakuyamon.conf")
(define /stone/sakuyamon.rsyslog (build-path (digimon-stone) "rsyslog.conf"))

(define sakuyamon.logadm "/etc/logadm.d/sakuyamon.conf")
(define /stone/sakuyamon.logadm (build-path (digimon-stone) "logadm.conf"))

(define sakuyamon.logrotate "/etc/logrotate.d/sakuyamon")
(define /stone/sakuyamon.logrotate (build-path (digimon-stone) "logrotate.conf"))

(define targets
  (case (digimon-system)
    [{solaris} (list sakuyamon.smf foxpipe.smf sakuyamon.rsyslog sakuyamon.logadm)]
    [{macosx} (list sakuyamon.plist foxpipe.plist sakuyamon.asl)]
    [{linux} (list sakuyamon.service foxpipe.service sakuyamon.rsyslog sakuyamon.logrotate)]))

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
      (make ([sakuyamon.plist [/sakuyamon/launchd.plist (quote-source-file) (find-executable-path "racket")]
                              (sudo.make sakuyamon.plist /sakuyamon/launchd.plist "root:wheel")]
             [foxpipe.plist [/foxpipe/launchd.plist (quote-source-file) (find-executable-path "racket")]
                            (sudo.make foxpipe.plist /foxpipe/launchd.plist "root:wheel")]
             [sakuyamon.smf [/sakuyamon/manifest.xml (quote-source-file) (find-executable-path "racket")]
                            (sudo.make sakuyamon.smf /sakuyamon/manifest.xml "root:sys")]
             [foxpipe.smf [/foxpipe/manifest.xml (quote-source-file) (find-executable-path "racket")]
                          (sudo.make foxpipe.smf /foxpipe/manifest.xml "root:sys")]
             [sakuyamon.service [/sakuyamon/systemd.service (quote-source-file) (find-executable-path "racket")]
                                (sudo.make sakuyamon.service /sakuyamon/systemd.service "root:root")]
             [foxpipe.service [/foxpipe/systemd.service (quote-source-file) (find-executable-path "racket")]
                              (sudo.make foxpipe.service /foxpipe/systemd.service "root:root")]
             [sakuyamon.asl [/stone/sakuyamon.asl (quote-source-file) (find-executable-path "racket")]
                            (and (sudo.make sakuyamon.asl /stone/sakuyamon.asl "root:wheel")
                                 (system "kill -s HUP `cat /var/run/syslog.pid`"))]
             [sakuyamon.rsyslog [/stone/sakuyamon.rsyslog (quote-source-file) (find-executable-path "racket")]
                                (and (sudo.make sakuyamon.rsyslog /stone/sakuyamon.rsyslog "root:root")
                                     (case (digimon-system)
                                       [{solaris} (system "svcadm restart system-log:rsyslog")]
                                       [{linux} (system "systemctl restart rsyslog")]))]
             [sakuyamon.logadm [/stone/sakuyamon.logadm (quote-source-file) (find-executable-path "racket")]
                               (sudo.make sakuyamon.logadm /stone/sakuyamon.logadm "root:sys")]
             [sakuyamon.logrotate [/stone/sakuyamon.logrotate (quote-source-file) (find-executable-path "racket")]
                                  (sudo.make sakuyamon.logrotate /stone/sakuyamon.logrotate "root:root")])
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
                    (path-only (car targets))))))
