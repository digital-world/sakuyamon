/* System Headers */
#if defined(__sun) && defined(__SVR4)
#define _POSIX_C_SOURCE 199506L
#define __EXTENSIONS__
#define __illumos__
#endif

#if defined(__APPLE__) && defined(__MACH__)
#define __macosx__
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <syslog.h>
#include <utmpx.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef __illumos__
#include <sys/loadavg.h>
#include <sys/swap.h>
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
#ifndef __linux__
static time_t boot_time = 0;
#endif

int system_statistics(long *ncores, double *avg01, double *avg05, double *avg15, time_t *uptime,
                      long *ramtotal, long *ramfree, long *swaptotal, long *swapfree) {
    intptr_t status;

#ifdef __linux__
    struct sysinfo info;
#endif

    errno = 0;

#ifdef __linux__
    status = sysinfo(&info);
    if (status == -1) goto job_done;
#endif

    /* number of processors */ {
        status = sysconf(_SC_NPROCESSORS_CONF);
        if (status == -1) goto job_done;

        (*ncores) = status;
    }

    /* system load average */ {
#ifndef __linux__
        double sysloadavg[3];

        status = getloadavg(sysloadavg, sizeof(sysloadavg) / sizeof(double));
        if (status == -1) goto job_done;

        (*avg01) = sysloadavg[0];
        (*avg05) = sysloadavg[1];
        (*avg15) = sysloadavg[2];
#else
        (*avg01) = info.loads[0];
        (*avg05) = info.loads[1];
        (*avg15) = info.loads[2];
#endif
    }

    /* uptime */ {
#ifdef __illumos__
        if (boot_time == 0) {
	        struct utmpx *btinfo, bthint;

            setutxent();
            bthint.ut_type = BOOT_TIME;
	        btinfo = getutxid(&bthint);
            if (btinfo != NULL) {
			    boot_time = btinfo->ut_tv.tv_sec;
                
                /**
                 * getting some types of record requires super privilege.
                 * but not for getting BOOT_TIME.
                 **/
                errno = 0;
            }
	        endutxent();

            if (errno != 0) goto job_done;
        }
	
        (*uptime) = time(NULL) - boot_time;
#endif

#ifdef __macosx__
        if (boot_time == 0) {
            struct timeval boottime;
            size_t size;

            size = sizeof(boottime);
            status = sysctlbyname("kern.boottime", &boottime, &size, NULL, 0);
            if ((status == -1) || (boottime.tv_sec == 0)) goto job_done;

            boot_time = boottime.tv_sec;
        }
        
        (*uptime) = time(NULL) - boot_time;
#endif

#ifdef __linux__
        (*uptime) = info.uptime;
#endif
    }

    /* memory status */ {
        long kb = 1024L;

#ifdef __illumos__
        struct anoninfo swapinfo;
        long pagesize, mtotal, mfree, stotal, sfree;
        
        pagesize = getpagesize();
        mtotal = sysconf(_SC_PHYS_PAGES);
        mfree = sysconf(_SC_AVPHYS_PAGES);
        if ((mtotal < 0) || (mfree < 0)) goto job_done;

        (*ramtotal) = pagesize * mtotal;
        (*ramfree) = pagesize * mfree;

        status = swapctl(SC_AINFO, &swapinfo);
        if (status == -1) goto job_done;

        /**
         * This algorithm relates to `swap -s`, see 'Solaris Performance and Tools'.
         **/
        (*swaptotal) = swapinfo.ani_max / kb * pagesize;
        (*swapfree) = (swapinfo.ani_max - swapinfo.ani_resv) / kb * pagesize;
#endif

#ifdef __macosx__
        struct xsw_usage swapinfo;
        long ram_raw, free_raw, speculative_raw;
        size_t size, pagesize;

        pagesize = getpagesize();
        size = sizeof(long);
        status = sysctlbyname("hw.memsize", &ram_raw, &size, NULL, 0);
        if (status == -1) goto job_done;
        size = sizeof(long);
        status = sysctlbyname("vm.page_free_count", &free_raw, &size, NULL, 0);
        if (status == -1) goto job_done;
        size = sizeof(long);
        status = sysctlbyname("vm.page_speculative_count", &speculative_raw, &size, NULL, 0);
        if (status == -1) goto job_done;

        (*ramtotal) = ram_raw / kb;
        
        /**
         * see `vm_stat`.c
         * TODO: These concepts are full of mystery.
         **/
        (*ramfree) = (free_raw - speculative_raw) / kb * pagesize;
        
        size = sizeof(struct xsw_usage);
        status = sysctlbyname("vm.swapusage", &swapinfo, &size, NULL, 0);
        if (status == -1) goto job_done;
        
        /**
         * TODO: I don't know whether the result should multiple swapinfo.xsu_pagesize.
         * No documents say it, but `sysctl`.c does not do.
         */
        (*swaptotal) = swapinfo.xsu_total / kb;
        (*swapfree) = swapinfo.xsu_avail / kb;
#endif

#ifdef __linux__
        (*ramtotal) = info.totalram / kb * info.mem_unit;
        (*ramfree) = info.freeram / kb * info.mem_unit;
        (*swaptotal) = info.totalswap / kb * info.mem_unit;
        (*swapfree) = info.freeswap / kb * info.mem_unit;
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

