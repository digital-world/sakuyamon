#!/usr/sbin/dtrace -s

#pragma D option quiet

int svc_startd;
int sub_startd;

dtrace:::BEGIN
{
    printf("svc.start: sakuyamon and foxpipe\n");
}

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
}

/** initialize **/

proc::cfork:create
/ ppid == 1 && execname == "svc.startd" /
{ 
    /* svc.startd */
    svc_startd = args[0]->pr_ppid;
    sub_startd = args[0]->pr_pid;
    smf[sub_startd, svc_startd] = 1;
    smf[sub_startd, sub_startd] = 1;
}

proc::cfork:create
/ smf[sub_startd, pid] == 1 /
{
    printf("%s[%d]: fork %s[%d:%d]: %s.\n",
            execname, pid, args[0]->pr_fname,
            args[0]->pr_pid, args[0]->pr_pgid,
            args[0]->pr_psargs);
    smf[sub_startd, args[0]->pr_pid] = 1;
}

/** monitor events and signals **/

proc::exec_common:exec
/ smf[sub_startd, pid] == 1 /
{
    printf("%s[%d]: exec %s.\n", execname, pid, args[0]);
}

proc::psig:signal-handle
/ smf[sub_startd, pid] == 1 && arg0 != 11 /
{
    /* filter out SIGSEGV */
    printf("%s[%d]: received SIG%s!\n", execname, pid, strsig[args[0]]);
}

proc::proc_exit:exit
/ smf[sub_startd, pid] == 1 /
{
    printf("%s[%d]: exited because of %s!\n", execname, pid, strchld[args[0]]);
    smf[sub_startd, pid] = 0;
}

/* Monitor TCP and UDP: Loop network results duplicate output. */
tcp:::send,
tcp:::receive
{
    printf("TCP@%d: %s:%d -> %s:%d %d (", cpu,
            args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport,
            args[2]->ip_plength - args[4]->tcp_offset);
    printf("%s", args[4]->tcp_flags & TH_FIN ? "FIN|" : "");
    printf("%s", args[4]->tcp_flags & TH_SYN ? "SYN|" : "");
    printf("%s", args[4]->tcp_flags & TH_RST ? "RST|" : "");
    printf("%s", args[4]->tcp_flags & TH_PUSH ? "PUSH|" : "");
    printf("%s", args[4]->tcp_flags & TH_ACK ? "ACK|" : "");
    printf("%s", args[4]->tcp_flags & TH_URG ? "URG|" : "");
    printf("%s", args[4]->tcp_flags & TH_ECE ? "ECE|" : "");
    printf("%s", args[4]->tcp_flags & TH_CWR ? "CWR|" : "");
    printf("%s", args[4]->tcp_flags == 0 ? "null " : "");
    printf("\b)\n");
}

udp:::receive
{
    printf("UDP@%d: %s:%d -> %s:%d %d\n", cpu,
            args[2]->ip_saddr, args[4]->udp_sport,
            args[2]->ip_daddr, args[4]->udp_dport,
            args[2]->ip_plength);
}

