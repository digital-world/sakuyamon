/* System Headers */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <syslog.h>
#include <stdarg.h>
#include <pwd.h>
#include <grp.h>

/* Quote Headers */

/* Typedef Declaration */

/* Constants Declaration */
#define BUFSIZE 32
static char lgnbuf[BUFSIZE];
static char grpbuf[BUFSIZE];

/* Procedures Implementation */

/** User and Group  **/
int fetch_tamer_ids(char* name, uid_t* uid, gid_t* gid) {
    struct passwd pwd, *passwords;
    char *buf;
    size_t bufsize;
    int status;

    errno = 0; /* make sure returned errno equals saved errno */
    bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
    if (bufsize == -1) bufsize = 16384;

    buf = malloc(bufsize);
    if (buf == NULL) goto exit_with_errno;

    status = getpwnam_r(name, &pwd, buf, bufsize, &passwords);
    if (passwords == NULL) {
        if (status == 0)
            errno = ENOENT;
        else {
            errno = status;
        }
        goto exit_with_errno;
    }

    (*uid) = pwd.pw_uid;
    (*gid) = pwd.pw_gid;

exit_with_errno:
    if (buf != NULL) free(buf);
    return errno;
}

int fetch_tamer_name(uid_t uid, char** login) {
    struct passwd *pwd;

    errno = 0; /* make sure returned errno equals saved errno */
    pwd = getpwuid(uid);
    if (pwd == NULL) goto exit_with_errno;

    strncpy(lgnbuf, pwd->pw_name, BUFSIZE);
    lgnbuf[BUFSIZE - 1] = '\0';

    (*login) = lgnbuf;

exit_with_errno:
    return errno;
}

int fetch_tamer_group(gid_t gid, char** group) {
    struct group *grp;

    errno = 0; /* make sure returned errno equals saved errno */
    grp = getgrgid(gid);
    if (grp == NULL) goto exit_with_errno;

    strncpy(grpbuf, grp->gr_name, BUFSIZE);
    grpbuf[BUFSIZE - 1] = '\0';

    (*group) = grpbuf;

exit_with_errno:
    return errno;
}

/* syslog */
void rsyslog(int priority, const char *topic, const char *message) {
    openlog("sakuyamon", LOG_PID | LOG_CONS, (getppid() == 1 ? LOG_DAEMON : LOG_USER));
    setlogmask(LOG_UPTO(LOG_INFO));
    syslog(priority, "%s: %s\n", topic, message);
    closelog();
}

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

