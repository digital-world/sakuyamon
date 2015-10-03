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
#include <libzfs.h> /* ld: ([illumos] . (zfs)) */
#include <libnvpair.h>
#include <sys/loadavg.h>
#include <sys/sysinfo.h>
#include <sys/swap.h>
#include <sys/fs/zfs.h>
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
#include <net/if.h>
#include <linux/if_link.h>
#include <ifaddrs.h>
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

#ifndef __linux__
static time_t boot_time = 0;
#endif

#ifdef __illumos__
static struct kstat_ctl *kstatistics = NULL;
static struct libzfs_handle *zfs = NULL;
static hrtime_t snaptime = 0ULL;
#endif

static int ncores = 0;
static size_t ramsize_kb = 0ULL;

static size_t disk_ikb = 0ULL;
static size_t disk_okb = 0ULL;
static size_t nic_ikb = 0ULL;
static size_t nic_okb = 0ULL;

#ifdef __illumos__
typedef struct zpool_iostat {
    size_t total;
    size_t used;
    size_t nread;
    size_t nwritten;
} zpool_iostat_t;

static int fold_zpool_iostat(zpool_handle_t *zthis, void *attachment) {
    zpool_iostat_t *iothis;
    boolean_t missing;

    iothis = (struct zpool_iostat *)attachment;
    zpool_refresh_stats(zthis, &missing);
    if (missing == B_FALSE) {
        nvlist_t *config, *zptree;
        int status, length;

        /**
         * see `zpool`_main.c
         *
         * libzfs is not a public API, meanwhile its internal data structure manages
         * both the latest status and the last status for calculating read/write rate.
         * So we just ignore the old status since we also manage it own out own.
         **/
        config = zpool_get_config(zthis, NULL);
        status = nvlist_lookup_nvlist(config, ZPOOL_CONFIG_VDEV_TREE, &zptree);
        if (status == 0) {
            vdev_stat_t *ioinfo;

            status = nvlist_lookup_uint64_array(zptree, ZPOOL_CONFIG_VDEV_STATS, (uint64_t **)&ioinfo, &length);
            if (status == 0) {
                iothis->total += ioinfo->vs_space;
                iothis->used += ioinfo->vs_alloc;
                iothis->nread += ioinfo->vs_bytes[ZIO_TYPE_READ];
                iothis->nwritten += ioinfo->vs_bytes[ZIO_TYPE_WRITE];
            }
        }
    }

    return 0;
}
#endif

char *system_statistics(time_t *timestamp, time_t *uptime,
        long *nprocessors, double *avg01, double *avg05, double *avg15,
        size_t *ramtotal, size_t *ramfree, size_t *swaptotal, size_t *swapfree,
        size_t *fstotal, size_t *fsfree, double *disk_ikbps, double *disk_okbps,
        uintmax_t *nic_in, double *nic_ikbps, uintmax_t *nic_out, double *nic_okbps) {
    char *alterrmsg;
    intptr_t status, pagesize;
    double duration_s;

#ifdef __macosx__
    size_t size;
#endif

#ifdef __linux__
    struct sysinfo info;

    status = sysinfo(&info);
    if (status == -1) goto job_done;
#endif

#ifdef __illumos__
    struct kstat *kthis;

    if (kstatistics == NULL) {
        kstatistics = kstat_open();
        if (kstatistics == NULL) goto job_done;
    } else {
        status = kstat_chain_update(kstatistics);
        if (status == -1) goto job_done;
    }

    if (zfs == NULL) {
        /* (zpool_iter) will always fold the chain in real time */
        zfs = libzfs_init();
        if (zfs == NULL) goto job_done;
    }
#endif

    /* static initialize */ {
        pagesize = getpagesize();

        if (ncores <= 0) {
            ncores = sysconf(_SC_NPROCESSORS_ONLN);
            if (ncores == -1) goto job_done;
        }

#ifdef __illumos__
        kthis = kstat_lookup(kstatistics, "unix", 0, "system_misc");
        if (kthis == NULL) goto job_done;
        status = kstat_read(kstatistics, kthis, NULL);
        if (status == -1) goto job_done;

        if (boot_time == 0) {
            kstat_named_t *nthis;

            nthis = (kstat_named_t *)kstat_data_lookup(kthis, "boot_time");
            if (nthis == NULL) goto job_done;
            boot_time = nthis->value.ui32;
        }

        if (ramsize_kb == 0) {
            size_t ram_raw;

            /* zone specific */
            ram_raw = sysconf(_SC_PHYS_PAGES);
            if (ram_raw < 0) goto job_done;

            ramsize_kb = ram_raw / kb * pagesize;
        }

        duration_s = (kthis->ks_snaptime - snaptime) / 1000.0 / 1000.0 / 1000.0;
        snaptime = kthis->ks_snaptime;
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

    /* simple output */ {
        errno = 0;
        alterrmsg = NULL;

        (*timestamp) = time(NULL);
        (*nprocessors) = ncores;
        (*ramtotal) = ramsize_kb;
        (*swaptotal) = 0ULL;
        (*swapfree) = 0ULL;
        (*fstotal) = 0ULL;
        (*fsfree) = 0ULL;
        (*nic_in) = 0ULL;
        (*nic_out) = 0ULL;

#ifdef __linux__
        (*uptime) = info.uptime;
#else
        (*uptime) = (*timestamp) - boot_time;
#endif
    }

    /* cpu and processes statistics */ {
        double sysloadavg[3];

        /**
         * TODO: Meanwhile the load average is good enough to
         *       show the status of usage and saturation.
         **/

        status = getloadavg(sysloadavg, sizeof(sysloadavg) / sizeof(double));
        if (status == -1) goto job_done;

        (*avg01) = sysloadavg[0];
        (*avg05) = sysloadavg[1];
        (*avg15) = sysloadavg[2];
    }

    /* memory statistics */ {
#ifdef __illumos__
        struct swaptable *stinfo;
        struct swapent *swap;
        int swapcount, stindex;

        /* zone specific */
        status = sysconf(_SC_AVPHYS_PAGES);
        if (status < 0) goto job_done;
        (*ramfree) = status / kb * pagesize;

        /**
         * The term swap in illumos relates to both anon pages and swapfs,
         * Here we only need swapfs just as it is in other Unices.
         * see `swap`.c
         **/

        swapcount = swapctl(SC_GETNSWP, NULL);
        if (swapcount == -1) goto job_done;
        if (swapcount > 0) {
            /* this elegant initialization is full of tricks [maybe stackoverflow]. */
            char storage[sizeof(int) + swapcount * sizeof(swapent_t)];
            char path[swapcount][MAXPATHLEN];

            stinfo = (struct swaptable *)&storage;
            stinfo->swt_n = swapcount;
            for (stindex = 0, swap = stinfo->swt_ent; stindex < swapcount; stindex ++, swap ++) {
                swap->ste_path = (char *)&path[stindex];
            }
            /* end of [maybe stackoverflow] */
            
            status = swapctl(SC_LIST, stinfo);
            if (status == -1) goto job_done;
            for (stindex = 0, swap = stinfo->swt_ent; stindex < swapcount; stindex ++, swap ++) {
                (*swaptotal) += swap->ste_pages * pagesize / kb;
                (*swapfree) += swap->ste_free * pagesize / kb;
            }
        }
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

        /*  see `vm_stat`.c */
        (*ramfree) = (free_raw - speculative_raw) / kb * pagesize;
        /* see `sysctl`.c.  NOTE: there is no need to multiple pagesize. */
        (*swaptotal) = swapinfo.xsu_total / kb;
        (*swapfree) = swapinfo.xsu_avail / kb;
#endif

#ifdef __linux__
        (*ramfree) = info.freeram / kb * info.mem_unit;
        (*swaptotal) = info.totalswap / kb * info.mem_unit;
        (*swapfree) = info.freeswap / kb * info.mem_unit;
#endif
    }

    /* disk statistics */ {
        uintmax_t disk_in, disk_out;

#ifdef __illumos__
        zpool_iostat_t zpiostat;

        /**
         * ZFS is one of the killer features of Illumos-based Operation System, and
         * the swapfs is also under control by zfs. TODO: For collecting simple samples,
         * to see if we still have to check the raw disk status.
         **/

        memset(&zpiostat, 0, sizeof(zpool_iostat_t));
        zpool_iter(zfs, fold_zpool_iostat, &zpiostat);
        errno = libzfs_errno(zfs);
        if (errno != 0) {
            alterrmsg = (char *)libzfs_error_description(zfs);
            goto job_done;
        }

        (*fstotal) = zpiostat.total / kb;
        (*fsfree) = (zpiostat.total - zpiostat.used) / kb;
        disk_in = zpiostat.nread / kb;
        disk_out = zpiostat.nwritten / kb;
#endif

        (*disk_ikbps) = (disk_in - disk_ikb) / duration_s;
        (*disk_okbps) = (disk_out - disk_okb) / duration_s;

        disk_ikb = disk_in;
        disk_okb = disk_out;
    }

    /* network statistics */ {
#ifdef __illumos__
        kstat_named_t *received, *sent;

        for (kthis = kstatistics->kc_chain; kthis !=NULL; kthis = kthis->ks_next) {
            if (strncmp(kthis->ks_module, "link", 5) == 0) { /* ks_class == "net" */
                status = kstat_read(kstatistics, kthis, NULL);
                if (status == -1) goto job_done;

                received = (kstat_named_t *)kstat_data_lookup(kthis, "rbytes64");
                if (received == NULL) goto job_done;
                sent = (kstat_named_t *)kstat_data_lookup(kthis, "obytes64");
                if (sent == NULL) goto job_done;

                (*nic_in) += received->value.ul / kb;
                (*nic_out) += sent->value.ul / kb;
            }
        }
#endif

#ifdef __macosx__
        struct ifmibdata ifinfo;
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
                (*nic_in) += ifinfo.ifmd_data.ifi_ibytes / kb;
                (*nic_out) += ifinfo.ifmd_data.ifi_obytes / kb;
            }
        }
#endif

#ifdef __linux__
        struct ifaddrs *ifinfo, *ifthis;
        struct rtnl_link_stats *ifstat;

        status = getifaddrs(&ifinfo);
        if (status == -1) goto job_done;

        for (ifthis = ifinfo; ifthis != NULL; ifthis = ifthis->ifa_next) {
            ifstat = (struct rtnl_link_stats *)ifthis->ifa_data;

            if ((ifstat != NULL) && !(ifthis->ifa_flags & IFF_LOOPBACK) && (ifthis->ifa_flags & IFF_RUNNING)
                    && (ifinfo->ifa_addr->sa_family == AF_PACKET)) {
                (*nic_in) += (uintmax_t)ifstat->rx_bytes / kb;
                (*nic_out) += (uintmax_t)ifstat->tx_bytes / kb;
            }
        }
        freeifaddrs(ifinfo);
#endif

        (*nic_ikbps) = ((*nic_in) - nic_ikb) / duration_s;
        (*nic_okbps) = ((*nic_out) - nic_okb) / duration_s;

        nic_ikb = (*nic_in);
        nic_okb = (*nic_out);
    }

job_done:
    return alterrmsg;
}

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

