#!/usr/sbin/dtrace -s

#pragma D option quiet

int izuna;
int port;

dtrace:::BEGIN
{
    printf("sakuyamon izuna [hostname] (via ssh direct channel)\n");
    izuna = 0;
    port = 0;
}

/* Initialize */
tcp:::connect-request
/ args[4]->tcp_dport == 22 /
{
    printf("TCP@%d: %s: %s:%d ~> %s:%d\n",
            args[1]->cs_pid, probename,
            args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport);
    izuna = args[1]->cs_pid;
    port = args[4]->tcp_sport;
}

/* this application may fetch ip geolocation via HTTP */
tcp:::connect-established
/ args[1]->cs_pid == izuna && args[4]->tcp_sport == 22 /
{
    printf("TCP@%d: %s: %s:%d <~ %s:%d\n",
            args[1]->cs_pid, probename,
            args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport);
}

tcp:::state-change
/ args[1]->cs_pid == izuna && args[3]->tcps_lport == port /
{
    printf("TCP@%4d: [%s:%d]: %s >> %s\n",
            args[1]->cs_pid,
            args[3]->tcps_laddr, args[3]->tcps_lport,
            tcp_state_string[args[5]->tcps_state],
            tcp_state_string[args[3]->tcps_state]);
}

/* Monitor TCP */
tcp:::send
/ args[1]->cs_pid == izuna && args[4]->tcp_dport == 22 /
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
/ args[1]->cs_pid == izuna && args[4]->tcp_sport == 22 /
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

/* Monitor Syscall */
/* It's useless to monitor send/recv since they are encrypted. */
syscall:::entry
/ pid == izuna /
{
    printf("%s\n", probefunc);
}

