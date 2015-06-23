#!/usr/sbin/dtrace -s

#pragma D option quiet

/*
 * why solaris daemon eats the whole CPU core
 * even when there is no request.
 */


dtrace:::BEGIN
{
    printf(" %3s %16s:%-5s    %16s:%-5s %6s %s\n",
            "CPU", "LADDR", "LPORT", "RADDR", "RPORT", "BYTES", "FLAGS");
}

tcp:::send
/ args[4]->tcp_dport == 80 /
{
    this->length = args[2]->ip_plength - args[4]->tcp_offset;
    printf(" %3d %16s:%-5d -> %16s:%-5d %6d (",
            cpu, args[2]->ip_saddr, args[4]->tcp_sport,
            args[2]->ip_daddr, args[4]->tcp_dport, this->length);
}

tcp:::receive
/ args[4]->tcp_sport == 80 /
{
    this->length = args[2]->ip_plength - args[4]->tcp_offset;
    printf(" %3d %16s:%-5d <- %16s:%-5d %6d (",
            cpu, args[2]->ip_daddr, args[4]->tcp_dport,
            args[2]->ip_saddr, args[4]->tcp_sport, this->length);
}

tcp:::send,
tcp:::receive
/ args[4]->tcp_sport == 80 || args[4]->tcp_dport == 80 /
{
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

