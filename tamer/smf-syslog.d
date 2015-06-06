#!/usr/sbin/dtrace -s

/*
 * The original purpose is to debug solaris smf method,
 * It can be also used to monitoring syslog dynamically.
 */

#pragma D option quiet

dtrace:::BEGIN
{
    printf("syslog monitor\n");
}

syscall::write:entry
/ ppid < 32 && execname == "racket" /
{
    printf("sakuyamon[%d]: %s", pid, copyinstr(arg1));
}

proc::psig:signal-handle
/ ppid < 32 && execname == "racket" && arg0 != 11 /
{
    /* filter out SIGSEGV */
    printf("sakuyamon[%d]: received signal %d!\n", pid, arg0);
}

