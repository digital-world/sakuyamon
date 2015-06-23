#!/bin/sh

svcadm_restart() {
    svcs sakuyamon >/dev/null 2>&1 && svcadm disable sakuyamon:default;
    svccfg import -V $1 || exit $?;
    if test "`svcs -H -o state sakuyamon:default`" != "online"; then
        svcadm enable sakuyamon:default;
    fi
}

launchctl_restart() {
    pkill -HUP -u `id -u tamer`;
    test $? -eq 0 || launchctl load $1;
}

systemctl_restart() {
    systemctl enable sakuyamon.service || exit $?;
    systemctl reload-or-restart sakuyamon.service;
}

case "$1" in
    solaris)
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

