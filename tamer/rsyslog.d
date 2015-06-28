#!/usr/sbin/dtrace -s

#pragma D option quiet


dtrace:::BEGIN
{
    printf("system-log:syslog\n");
}

/** Monitor rsyslogd **/

syscall::open*:entry
/ execname == "rsyslogd" && substr(copyinstr(arg0), 0, 4) == "/etc" /
{
    printf("[<< file://%s]\n", copyinstr(arg0));
}

/* Monitor UDP forwarding for foxpipe */
udp:::send
/ execname == "rsyslogd" /
{
    printf("[>> udp://%s:%d]\n", args[2]->ip_daddr, args[4]->udp_dport);
}

/* Simple alternative to gnome-system-log */

syscall::write:entry
/ execname == "rsyslogd" /
{
    printf("%s", copyinstr(arg1));
}

