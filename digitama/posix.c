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
#include <math.h>

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
#define ROUND(x, m) (((long)round((x) * m)) / (m * 1.0))
#define kb (1024) /* use MB directly may lose precision */

static int ncores = 0;
static long ramsize_kb = 0L;

static hrtime_t link_snaptime = 0L;
static long link_rbytes = 0L;
static long link_obytes = 0L;

#ifndef __linux__
static time_t boot_time = 0;
#endif

#ifdef __illumos__
static struct kstat_ctl *kstatistics = NULL;
#endif

int system_statistics(time_t *timestamp, time_t *uptime,
                      long *nprocessors, double *avg01, double *avg05, double *avg15,
                      long *ramtotal, long *ramfree, long *swaptotal, long *swapfree,
                      long *rbytes, double *rkbps, long *obytes, double *okbps) {
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

        (*avg01) = ROUND(sysloadavg[0], 100);
        (*avg05) = ROUND(sysloadavg[1], 100);
        (*avg15) = ROUND(sysloadavg[2], 100);
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
#ifdef __illumos__
        kstat_t *syslink;
        kstat_named_t *recv, *send;
        double delta_time;

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

        (*rbytes) = recv->value.ul;
        (*obytes) = send->value.ul;

        delta_time = (syslink->ks_snaptime - link_snaptime) / 1000.0 / 1000.0 / 1000.0;
        (*rkbps) = ROUND(((*rbytes) - link_rbytes) / delta_time / kb, 1000);
        (*okbps) = ROUND(((*obytes) - link_obytes) / delta_time / kb, 1000);
        
        link_snaptime = syslink->ks_snaptime;
        link_rbytes = (*rbytes);
        link_obytes = (*obytes);
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

