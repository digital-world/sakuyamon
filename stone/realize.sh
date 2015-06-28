#!/bin/sh

svcadm_restart() {
    svcs sakuyamon >/dev/null 2>&1 && svcadm disable sakuyamon:default;
    svccfg import -V $1/sakuyamon.xml || exit $?;
    if test "`svcs -H -o state sakuyamon:default`" != "online"; then
        svcadm enable sakuyamon:default;
    fi

    svcs foxpipe >/dev/null 2>&1 && svcadm disable foxpipe:default;
    svccfg import -V $1/foxpipe.xml || exit $?;
    if test "`svcs -H -o state foxpipe:default`" != "online"; then
        svcadm enable foxpipe:default;
    fi
}

launchctl_restart() {
    pkill -HUP -u `id -u tamer`;
    test $? -eq 0 || launchctl load $1;
}

systemctl_restart() {
    systemctl enable sakuyamon.service || exit $?;
    systemctl reload-or-restart sakuyamon.service;

    systemctl enable foxpipe.service || exit $?;
    systemctl reload-or-restart foxpipe.service;
}

daemonize_foxpipe() {
    $1/sakuyamon.rkt foxpipe &
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
    foxpipe)
        daemonize_foxpipe $2;
        ;;
    *)
        false;
        ;;
esac

