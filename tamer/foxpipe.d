#!/usr/sbin/dtrace -s

#pragma D option quiet

int port;

dtrace:::BEGIN
{
    printf("sakuyamon Kudagitsune!\n");
    port = 514;
}

/* Monitor TCP */
tcp:::send
/ args[4]->tcp_dport == port /
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

tcp:::receive
/ args[4]->tcp_sport == port /
{
    printf("TCP@%d: %s:%d <- %s:%d %d (", cpu,
            args[2]->ip_daddr, args[4]->tcp_dport,
            args[2]->ip_saddr, args[4]->tcp_sport,
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
/ args[4]->udp_dport == port /
{
    printf("UDP@%d: %s:%d <- %s:%d %d\n", cpu,
            args[2]->ip_daddr, args[4]->udp_dport,
            args[2]->ip_saddr, args[4]->udp_sport,
            args[2]->ip_plength);
}

