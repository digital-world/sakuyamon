#!/usr/sbin/dtrace -s

#pragma D option quiet

/*
 * Simple alternative to gnome-system-log
 */


dtrace:::BEGIN
{
    printf("system-log:syslog\n");
}

/** monitor syslog **/

syscall::open*:entry
/ execname == "rsyslogd" && substr(copyinstr(arg0), 0, 4) == "/etc" /
{
    printf("[>> %s <<]\n", copyinstr(arg0));
}

syscall::write:entry
/ execname == "rsyslogd" /
{
    printf("%s", copyinstr(arg1));
}

