/* System Headers */
#if defined(__sun) && defined(__SVR4)
#define _POSIX_C_SOURCE 199506L
#define __EXTENSIONS__
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

#if defined(__sun) && defined(__SVR4)
#include <sys/loadavg.h>
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
static time_t boot_time = 0;

int vector_get_performance_stats(long *ncores, double *avg1, double *avg5, double *avg15, time_t *uptime) {
    intptr_t status;

    errno = 0;

    /* number of processors */ {
        status = sysconf(_SC_NPROCESSORS_CONF);
        if (status == -1) goto job_done;

        (*ncores) = status;
    }

    /* system load average */ {
        double sysloadavg[3];

        status = getloadavg(sysloadavg, sizeof(sysloadavg) / sizeof(double));
        if (status == -1) goto job_done;

        (*avg1) = sysloadavg[0];
        (*avg5) = sysloadavg[1];
        (*avg15) = sysloadavg[2];
    }

    /* uptime:illumos-gate:w.c */ {
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
    }

job_done:
    return errno;
}

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

