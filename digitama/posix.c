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
#include <kern/clock.h>
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
#ifdef __illumos__
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
        clock_get_uptime(uptime);
#endif

#ifdef __linux__
        (*uptime) = info.uptime;
#endif
    }

    /* memory status */ {
#ifdef __illumos__
        struct anoninfo swapinfo;
        long pagesize, mtotal, mfree, stotal, sfree;
        long kb = 1024L;
        
        pagesize = getpagesize();
        mtotal = sysconf(_SC_PHYS_PAGES);
        mfree = sysconf(_SC_AVPHYS_PAGES);
        if ((mtotal < 0) || (mfree < 0)) goto job_done;

        (*ramtotal) = pagesize * mtotal;
        (*ramfree) = pagesize * mfree;

        status = swapctl(SC_AINFO, &swapinfo);
        if (status == -1) goto job_done;

        /**
         * Note: The Virtual Memory of Illumos is different from other Unices.
         * This algorithm relates to `swap -s`, see 'Solaris Performance and Tools'.
         **/
        (*swaptotal) = swapinfo.ani_max / kb * pagesize;
        (*swapfree) = (swapinfo.ani_max - swapinfo.ani_resv) / kb * pagesize;
#endif

#if __macosx__
        intptr_t mib[2];
        size_t size;

        mib[0] = CTL_HW;
        mib[1] = HW_MEMSIZE;
        status = sysctl(mib, 2, ramtotal, &size, NULL, 0);
        if (status == -1) goto job_done;
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

