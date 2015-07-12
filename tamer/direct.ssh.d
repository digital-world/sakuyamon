#!/usr/sbin/dtrace -s

#pragma D option quiet

int port;
int foxpipe;

dtrace:::BEGIN
{
    printf("sakuyamon Kudagitsune (via ssh direct channel)\n");
    port = 514;
    sshd[-1] = 1;
}

dtrace:::BEGIN
{
    strsig[1] = "HUP";
    strsig[2] = "INT";
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
/ execname == "sshd" /
{
    printf("%s[%d]: %s: %s[%d@%d]: %s.\n",
            execname, pid, probefunc,
            args[0]->pr_fname, args[0]->pr_pid,
            args[0]->pr_pgid, args[0]->pr_psargs);
    sshd[args[0]->pr_pid] = 1;
}

/** monitor events and signals **/

proc::exec_common:exec
/ sshd[pid] == 1 /
{
    printf("%s[%d]: %s: %s.\n", execname, pid, probefunc, args[0]);
}

proc::psig:signal-handle
/ sshd[pid] == 1 && arg0 != 11 /
{
    /* filter out SIGSEGV */
    printf("%s[%d]: %s: SIG%s!\n", execname, pid, probefunc, strsig[args[0]]);
}

proc::proc_exit:exit
/ sshd[pid] == 1 /
{
    printf("%s[%d]: %s[%s]!\n", execname, pid, probefunc, strchld[args[0]]);
    sshd[pid] = 0;
}

/* Monitor TCP */
tcp:::state-change
{
    printf("TCP@%4d: [%s:%d]: %s >> %s\n",
            args[1]->cs_pid,
            args[3]->tcps_laddr, args[3]->tcps_lport,
            tcp_state_string[args[5]->tcps_state],
            tcp_state_string[args[3]->tcps_state]);
}

tcp:::connect-request,
tcp:::connect-established,
tcp:::connect-refused
{
    printf("TCP@%d: %s: %s:%d ~> %s:%d\n",
            args[1]->cs_pid, probename,
            args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport);
}

tcp:::accept-established,
tcp:::accept-refused
{
    printf("TCP@%d: %s: %s:%d <~ %s:%d\n",
            args[1]->cs_pid, probename,
            args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport);
}

tcp:::send
/ sshd[args[1]->cs_pid] == 1 /
{
    printf("TCP@%d: %s[%d]: %s:%d => %s:%d (",
            args[1]->cs_pid, probename,
            args[2]->ip_plength - args[4]->tcp_offset,
            args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport);
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

tcp:::receive
/ sshd[args[1]->cs_pid] == 1 /
{
    printf("TCP@%d: %s[%d]: %s:%d <= %s:%d (",
            args[1]->cs_pid, probename,
            args[2]->ip_plength - args[4]->tcp_offset,
            args[2]->ip_daddr, args[4]->tcp_dport,
            args[2]->ip_saddr, args[4]->tcp_sport);
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
/ args[4]->udp_dport == port /
{
    printf("UDP@%d: %s[%d]: %s:%d <= %s:%d\n",
            args[1]->cs_pid, probename, args[2]->ip_plength,
            args[2]->ip_daddr, args[4]->udp_dport,
            args[2]->ip_saddr, args[4]->udp_sport);
    sshd[args[1]->cs_pid];
    foxpipe = args[1]->cs_pid;
}

