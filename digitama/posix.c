/* System Headers */
#if defined(__sun) && defined(__SVR4)
#define _POSIX_C_SOURCE 199506L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <syslog.h>
#include <stdarg.h>
#include <pwd.h>
#include <grp.h>

/* Quote Headers */

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
double sysloadavg[3];

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

