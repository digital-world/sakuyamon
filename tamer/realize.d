#!/usr/sbin/dtrace -s

/*
 * The original purpose is tracing solaris smf method,
 * It can be also used to monitoring syslog dynamically.
 *
 * TODO: Maybe BUGS?
 * Service is working, but keeps forking and exiting.
 * Process Group?
 * 
 */

#pragma D option quiet

int svc_startd;

dtrace:::BEGIN
{
    strsig[1] = "SIGHUP";
    strsig[2] = "SIGINT";
    strsig[3] = "QUIT";
    strsig[4] = "ILL";
    strsig[5] = "TRAP";
    strsig[6] = "IOT";
    strsig[7] = "EMT";
    strsig[8] = "FPE";
    strsig[9] = "KILL";
    strsig[10] = "BUS";
    strsig[11] = "SEGV";
    strsig[12] = "SYS";
    strsig[13] = "PIPE";
    strsig[14] = "ALRM";
    strsig[15] = "TERM";
    strsig[16] = "USR1";
    strsig[17] = "USR2";
    strsig[18] = "CHLD";
    strsig[19] = "PWR";
    strsig[20] = "WINCH";
    strsig[21] = "URG";
    strsig[22] = "POLL";
    strsig[23] = "STOP";
    strsig[24] = "TSTP";
    strsig[25] = "CONT";
    strsig[26] = "TTIN";
    strsig[27] = "TTOU";
    strsig[28] = "VTALRM";
    strsig[29] = "PROF";
    strsig[30] = "XCPU";
    strsig[31] = "XFSZ";
    strsig[32] = "WAITING";
    strsig[33] = "LWP";
    strsig[34] = "FREEZE";
    strsig[35] = "THAW";
    strsig[36] = "CANCEL";
    strsig[37] = "LOST";
    strsig[38] = "XRES";
    strsig[39] = "JVM1";
    strsig[40] = "JVM2";
    strsig[41] = "INFO";
    strsig[42] = "RTMIN";
    strchld[1] = "CLD_EXITED";
    strchld[2] = "CLD_KILLED";
    strchld[3] = "CLD_DUMPED";
    strchld[4] = "CLD_TRAPPED";
    strchld[5] = "CLD_STOPPED";
    strchld[6] = "CLD_CONTINUED";

    printf("svc.startd: syslog monitor\n");
}

/** initialize **/

proc::cfork:create
/ ppid == 1 /
{ 
    /* svc.startd */
    svc_startd = args[0]->pr_ppid;
    smf[args[0]->pr_pid] = 1;
    smf[svc_startd] = 1;
}

proc::cfork:create
/ smf[pid] == 1 /
{
    printf("%s[%d]: fork %s[%d:%d]: %s.\n", execname, pid, args[0]->pr_fname,
                                            args[0]->pr_pid, args[0]->pr_pgid,
                                            args[0]->pr_psargs);
    smf[args[0]->pr_pid] = 1;
}

/** monitor events and signals **/

proc::exec_common:exec
/ smf[pid] == 1 /
{
    printf("%s[%d]: exec %s.\n", execname, pid, args[0]);
}

proc::psig:signal-handle
/ smf[pid] == 1 && arg0 != 11 /
{
    /* filter out SIGSEGV */
    printf("%s[%d]: received SIG%s!\n", execname, pid, strsig[args[0]]);
}

proc::proc_exit:exit
/ smf[pid] == 1 /
{
    printf("%s[%d]: exited because of %s!\n", execname, pid, strchld[args[0]]);
    smf[pid] = 0;
}

/** monitor syslog **/

syscall::write:entry
/ execname == "racket" && strlen(copyinstr(arg1)) > strlen("sakuyamon") /
{
    /* Also trace makefile.rkt */
    printf("%s[%d]: %s", execname, pid, copyinstr(arg1));
}

