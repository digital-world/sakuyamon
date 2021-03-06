#!/bin/sh

svcadm_restart() {
    svcs sakuyamon >/dev/null 2>&1 && svcadm disable sakuyamon:default;
    svccfg import -V $1/sakuyamon.xml || exit $?;
    if test "`svcs -H -o state sakuyamon:default`" != "online"; then
        svcadm enable sakuyamon:default || exit $?;
    fi

    svcs foxpipe >/dev/null 2>&1 && svcadm disable foxpipe:default;
    svccfg import -V $1/foxpipe.xml || exit $?;
    if test "`svcs -H -o state foxpipe:default`" != "online"; then
        svcadm enable foxpipe:default || exit $?;
    fi
}

launchctl_restart() {
    pkill -HUP -u `id -u tamer`;
    launchctl list | grep sakuyamon || launchctl load $1/org.gyoudmon.sakuyamon.plist;
    launchctl list | grep foxpipe || launchctl load $1/org.gyoudmon.foxpipe.plist;
}

systemctl_restart() {
    systemctl enable sakuyamon.service;
    systemctl reload sakuyamon.service;
    systemctl restart sakuyamon.service || exit $?;

    systemctl enable foxpipe.service;
    systemctl reload foxpipe.service;
    systemctl restart foxpipe.service || exit $?;
}

case "$1" in
    illumos)
        svcadm_restart $2;
        ;;
    macosx)
        launchctl_restart $2; 
        ;;
    linux)
        systemctl_restart $2;
        ;;
    *)
        false;
        ;;
esac

