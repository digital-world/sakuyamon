/* System Headers */
#ifdef __linux__
#define _BSD_SOURCE
#endif

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <syslog.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef __illumos__
#include <kstat.h> /* ld: ([illumos] . (kstat)) */
#include <sys/loadavg.h>
#include <sys/swap.h>
#include <sys/sysinfo.h>
#include <vm/anon.h>
#endif

#ifdef __macosx__
#include <sys/sysctl.h>
#include <sys/socket.h>
#include <net/if.h>
#include <net/if_mib.h>
#include <net/if_var.h>
#include <net/if_types.h>
#endif

#ifdef __linux__
#include <sys/sysinfo.h>
#endif

/** Syslog Logs **/
void rsyslog(int priority, const char *topic, const char *message) {
    int facility;

    if (getuid() == 0) { /* svc.startd's pid is 10 rather than 1 */
        facility = LOG_DAEMON;
    } else {
        facility = LOG_USER;
    }
                         
    
    openlog("sakuyamon", LOG_PID | LOG_CONS, facility);
    setlogmask(LOG_UPTO(LOG_DEBUG));
    syslog(priority, "%s: %s\n", topic, message);
    closelog();
}

/** System Monitor **/
#define kb (1024) /* use MB directly may lose precision */

static int ncores = 0;
static long ramsize_kb = 0L;

#ifdef __illumos__
static hrtime_t nic_snaptime = 0L;
#else
static uintmax_t nic_snaptime = 0L;
#endif

static size_t nic_ibytes = 0L;
static size_t nic_obytes = 0L;

#ifndef __linux__
static time_t boot_time = 0;
#endif

#ifdef __illumos__
static struct kstat_ctl *kstatistics = NULL;
#endif

int system_statistics(time_t *timestamp, time_t *uptime,
                      long *nprocessors, double *avg01, double *avg05, double *avg15,
                      size_t *ramtotal, size_t *ramfree, size_t *swaptotal, size_t *swapfree,
                      size_t *nic_in, double *nic_ikbps, size_t *nic_out, double *nic_okbps) {
    intptr_t status, pagesize;

#ifdef __macosx__
    size_t size;
#endif

#ifdef __linux__
    struct sysinfo info;
    
    status = sysinfo(&info);
    if (status == -1) goto job_done;
#endif

#ifdef __illumos__
    if (kstatistics == NULL) {
        kstatistics = kstat_open();
        if (kstatistics == NULL) goto job_done;
    } else {
        status = kstat_chain_update(kstatistics);
        if (status == -1) goto job_done;
    }
#endif

    /* static initialize */ {
        pagesize = getpagesize();

        if (ncores <= 0) {
            ncores = sysconf(_SC_NPROCESSORS_ONLN);
            if (ncores == -1) goto job_done;
        }

#ifdef __illumos__
        if (boot_time == 0) {
            kstat_t *sysmisc;
            kstat_named_t *dt;

            sysmisc = kstat_lookup(kstatistics, "unix", 0, "system_misc");
            if (sysmisc == NULL) goto job_done;
            status = kstat_read(kstatistics, sysmisc, NULL);
            if (status == -1) goto job_done;

            dt = (kstat_named_t *)kstat_data_lookup(sysmisc, "boot_time");
            if (dt == NULL) goto job_done;
            boot_time = dt->value.ui32;
        }

        if (ramsize_kb == 0) {
            size_t ram_raw;
            
            /* a little larger than kstat:unix:system_pages:physmem */
            ram_raw = sysconf(_SC_PHYS_PAGES);
            if (ram_raw < 0) goto job_done;
            
            ramsize_kb = ram_raw / kb * pagesize;
        }
#endif

#ifdef __macosx__
        if (boot_time == 0) {
            struct timeval boottime;

            size = sizeof(struct timeval);
            status = sysctlbyname("kern.boottime", &boottime, &size, NULL, 0);
            if ((status == -1) || (boottime.tv_sec == 0)) goto job_done;

            boot_time = boottime.tv_sec;
        }

        if (ramsize_kb == 0) {
            size_t ram_raw;

            size = sizeof(size_t);
            status = sysctlbyname("hw.memsize", &ram_raw, &size, NULL, 0);
            if (status == -1) goto job_done;
            
            ramsize_kb = ram_raw / kb;
        }
#endif

#ifdef __linux__
        if (ramsize_kb == 0) {
            ramsize_kb = info.totalram / kb * info.mem_unit;
        }
#endif
    }

    /* output predefined constants */ {
        errno = 0;

        (*nprocessors) = ncores;
        (*ramtotal) = ramsize_kb;
    }

    /* timestamp and uptime */ {
        (*timestamp) = time(NULL);

#ifdef __linux__
        (*uptime) = info.uptime;
#else
        (*uptime) = (*timestamp) - boot_time;
#endif
    }

    /* system load average */ {
        double sysloadavg[3];

        status = getloadavg(sysloadavg, sizeof(sysloadavg) / sizeof(double));
        if (status == -1) goto job_done;

        (*avg01) = sysloadavg[0];
        (*avg05) = sysloadavg[1];
        (*avg15) = sysloadavg[2];
    }

    /* cpu and processes statistics */ {
        /**
         * TODO: Meanwhile the load average is good enough to
         *       show the status of usage and saturation.
         **/
    }

    /* memory statistics */ {
#ifdef __illumos__
        struct anoninfo swapinfo;
        size_t mfree, stotal, sfree;

        /* smaller than kstat:unix:system_pages:availrmem */
        /* a little larger than kstat:unix:system_pages:pagesfree/freemem */
        mfree = sysconf(_SC_AVPHYS_PAGES);
        if (mfree < 0) goto job_done;
        status = swapctl(SC_AINFO, &swapinfo);
        if (status == -1) goto job_done;

        (*ramfree) = mfree / kb * pagesize;

        /**
         * This algorithm relates to `swap -s`, see 'Solaris Performance and Tools'.
         **/
        (*swaptotal) = swapinfo.ani_max / kb * pagesize;
        (*swapfree) = (swapinfo.ani_max - swapinfo.ani_resv) / kb * pagesize;
#endif

#ifdef __macosx__
        struct xsw_usage swapinfo;
        size_t free_raw, speculative_raw;

        size = sizeof(size_t);
        status = sysctlbyname("vm.page_free_count", &free_raw, &size, NULL, 0);
        if (status == -1) goto job_done;
        size = sizeof(size_t);
        status = sysctlbyname("vm.page_speculative_count", &speculative_raw, &size, NULL, 0);
        if (status == -1) goto job_done;
        size = sizeof(struct xsw_usage);
        status = sysctlbyname("vm.swapusage", &swapinfo, &size, NULL, 0);
        if (status == -1) goto job_done;
        
        /**
         * see `vm_stat`.c
         * TODO: These concepts are full of mystery.
         **/
        (*ramfree) = (free_raw - speculative_raw) / kb * pagesize;
        
        /**
         * see `sysctl`.c.
         * NOTE: there is no need to multiple pagesize.
         */
        (*swaptotal) = swapinfo.xsu_total / kb;
        (*swapfree) = swapinfo.xsu_avail / kb;
#endif

#ifdef __linux__
        (*ramfree) = info.freeram / kb * info.mem_unit;
        (*swaptotal) = info.totalswap / kb * info.mem_unit;
        (*swapfree) = info.freeswap / kb * info.mem_unit;
#endif
    }

    /* network statistics */ {
        double delta_time;

#ifdef __illumos__
        kstat_t *syslink;
        kstat_named_t *recv, *send;

        /**
         * TODO: if there are more physic netword interfaces.
         */
        syslink = kstat_lookup(kstatistics, "link", 0, NULL);
        if (syslink == NULL) goto job_done;
        status = kstat_read(kstatistics, syslink, NULL);
        if (status == -1) goto job_done;

        recv = (kstat_named_t *)kstat_data_lookup(syslink, "rbytes64");
        if (recv == NULL) goto job_done;
        send = (kstat_named_t *)kstat_data_lookup(syslink, "obytes64");
        if (send == NULL) goto job_done;

        (*nic_in) = recv->value.ul;
        (*nic_out) = send->value.ul;

        delta_time = (syslink->ks_snaptime - nic_snaptime) / 1000.0 / 1000.0 / 1000.0;
        (*nic_ikbps) = ((*nic_in) - nic_ibytes) / delta_time / kb;
        (*nic_okbps) = ((*nic_out) - nic_obytes) / delta_time / kb;
        
        nic_snaptime = syslink->ks_snaptime;
        nic_ibytes = (*nic_in);
        nic_obytes = (*nic_out);
#endif

#ifdef __macosx__
        struct ifmibdata ifinfo;
        struct timeval snaptime;
        uintmax_t snaptime_us;
        size_t size, ifindex /*, ifcount */;
        int ifmib[6];

        size = sizeof(size_t);
        ifmib[0] = CTL_NET;
        ifmib[1] = PF_LINK;
        ifmib[2] = NETLINK_GENERIC;

        /**
         * ifmib[3] = IFMIB_SYSTEM;
         * ifmib[4] = IFMIB_IFCOUNT;
         * status = sysctl(ifmib, 5, &ifcount, &size, NULL, 0);
         * if (status == -1) goto job_done;
         * 
         * This should be the standard way,
         * but weird, the `for` statement does not stop when ifindex < ifcount.
         */

        (*nic_in) = 0L;
        (*nic_out) = 0L;

        ifmib[3] = IFMIB_IFDATA;
        ifmib[5] = IFDATA_GENERAL;
        for (ifindex = 1 /* see `man ifmib` */; ifindex /* < ifcount */; ifindex ++) {
            size = sizeof(struct ifmibdata);
            ifmib[4] = ifindex;

            status = sysctl(ifmib, 6, &ifinfo, &size, NULL, 0);
            if (status == -1) {
                if (errno == ENOENT) {
                    /* this should not happen unless the weird bug appears. */
                    errno = 0;
                    break;
                }
                goto job_done;
            } 

            if (ifinfo.ifmd_data.ifi_type == IFT_ETHER) {
                (*nic_in) += ifinfo.ifmd_data.ifi_ibytes;
                (*nic_out) += ifinfo.ifmd_data.ifi_obytes;
            }
        }

        gettimeofday(&snaptime, NULL);
        snaptime_us = (snaptime.tv_sec * 1000 * 1000) + snaptime.tv_usec;
        delta_time = (snaptime_us - nic_snaptime) / 1000.0 / 1000.0;
        (*nic_ikbps) = ((*nic_in) - nic_ibytes) / delta_time / kb;
        (*nic_okbps) = ((*nic_out) - nic_obytes) / delta_time / kb;
        
        nic_snaptime = snaptime_us;
        nic_ibytes = (*nic_in);
        nic_obytes = (*nic_out);
#endif
    }

job_done:
    return errno;
}

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

