#!/bin/sh

svcadm_restart() {
    svcs sakuyamon >/dev/null 2>&1 || svcadm disable sakuyamon:default;
    svccfg import -V $1 || exit 1;
    if test "`svcs -H -o state sakuyamon:default`" != "online"; then
        svcadm enable sakuyamon:default;
    fi
}

launchctl_restart() {
    pkill -HUP -u `id -u tamer`;
    test $0 -eq 0 || launchctl load $1;
}

case "$1" in
    solaris)
        svcadm_restart $2;
        ;;
    macosx)
        launchctl_restart $2;
        ;;
    *)
        false;
        ;;
esac

