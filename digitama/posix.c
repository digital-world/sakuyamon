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
#include <kstat.h> /* ld:illumos: (kstat) */
#include <libzfs.h> /* ld:illumos: (zfs) */
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
#include <mach/mach_host.h>
#include <mach/vm_statistics.h>
#define IOKIT 1 /* for io_name_t */
#include <sys/param.h>
#include <sys/ucred.h>
#include <sys/mount.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h> /* ld:framework: (CoreFoundation Foundation IOKit) */
#include <IOKit/storage/IOBlockStorageDriver.h>
#include <IOKit/storage/IOMedia.h>
#include <IOKit/IOReturn.h>
#include <IOKit/IOBSD.h>
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

#ifdef __illumos__
static struct kstat_ctl *kstatistics = NULL;
static struct libzfs_handle *zfs = NULL;
static hrtime_t snaptime = 0ULL;
#endif

#ifdef __macosx__
static uintmax_t snaptime = 0ULL;
static mach_port_t localhost, iostat;
#endif

#ifdef __linux__
static uintmax_t snaptime = 0ULL;
#else
static time_t boot_time = 0;
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
    size_t sysdatum_size;
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

        /* illumos zone can change cpu or core online */ {
            kstat_named_t *nthis;

            nthis = (kstat_named_t *)kstat_data_lookup(kthis, "ncpus");
            if (nthis == NULL) goto job_done;
            ncores = nthis->value.ui32;
        }

        /* illumos zone can change physical memory online */ {
            size_t ram_raw;

            /* zone specific */
            ram_raw = sysconf(_SC_PHYS_PAGES);
            if (ram_raw < 0) goto job_done;

            ramsize_kb = ram_raw * pagesize / kb;
        }

        duration_s = (kthis->ks_snaptime - snaptime) / 1000.0 / 1000.0 / 1000.0;
        snaptime = kthis->ks_snaptime;
#endif

#ifdef __macosx__
        if (boot_time == 0) {
            struct timeval boottime;

            sysdatum_size = sizeof(struct timeval);
            status = sysctlbyname("kern.boottime", &boottime, &sysdatum_size, NULL, 0);
            if ((status == -1) || (boottime.tv_sec == 0)) goto job_done;

            boot_time = boottime.tv_sec;
        }

        if (ncores == 0) {
            size_t ncpu;

            sysdatum_size = sizeof(size_t);
            status = sysctlbyname("hw.ncpu", &ncpu, &sysdatum_size, NULL, 0);
            if (status == -1) goto job_done;

            ncores = ncpu;
        }

        if (ramsize_kb == 0) {
            size_t ram_raw;

            sysdatum_size = sizeof(size_t);
            status = sysctlbyname("hw.memsize", &ram_raw, &sysdatum_size, NULL, 0);
            if (status == -1) goto job_done;

            ramsize_kb = ram_raw / kb;
        }

        if (localhost == 0) {
            localhost = mach_host_self();
        }

        if (iostat == 0) {
            errno = IOMasterPort(bootstrap_port, &iostat);
            if (errno != KERN_SUCCESS) goto job_done;
        }
        
        {
            struct timeval now;
            uintmax_t now_us;

            gettimeofday(&now, NULL);
            
            now_us = now.tv_sec * 1000 * 1000 + now.tv_usec;
            duration_s = (now_us - snaptime) / 1000.0 / 1000.0;
            snaptime = now_us;
        }
#endif

#ifdef __linux__
        if (ncores <= 0) {
            ncores = sysconf(_SC_NPROCESSORS_ONLN);
            if (ncores == -1) goto job_done;
        }

        if (ramsize_kb == 0) {
            ramsize_kb = info.totalram * info.mem_unit / kb;
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

    /* memory and swapfs statistics */ {
#ifdef __illumos__
        struct swaptable *stinfo;
        struct swapent *swap;
        int swapcount, stindex;

        /* zone specific */
        status = sysconf(_SC_AVPHYS_PAGES);
        if (status < 0) goto job_done;
        (*ramfree) = status * pagesize / kb;

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
        struct vm_statistics vminfo;
        mach_msg_type_number_t count;
        struct xsw_usage swapinfo;
        
        bzero(&vminfo, sizeof(vminfo));
        count = HOST_VM_INFO_COUNT;
        errno = host_statistics(localhost, HOST_VM_INFO, (host_info64_t)&vminfo, &count);
        if (errno != KERN_SUCCESS) goto job_done;
        /**
         * see vm_statistics.h
	     * NB: speculative pages are already accounted for in "free_count",
	     * so "speculative_count" is the number of "free" pages that are
	     * used to hold data that was read speculatively from disk but
	     * haven't actually been used by anyone so far.
         **/
        (*ramfree) = (vminfo.free_count - vminfo.speculative_count) * pagesize / kb;

        sysdatum_size = sizeof(struct xsw_usage);
        status = sysctlbyname("vm.swapusage", &swapinfo, &sysdatum_size, NULL, 0);
        if (status == -1) goto job_done;
        /* see `sysctl`.c.  NOTE: there is no need to multiple pagesize. */
        (*swaptotal) = swapinfo.xsu_total / kb;
        (*swapfree) = swapinfo.xsu_avail / kb;
#endif

#ifdef __linux__
        (*ramfree) = info.freeram * info.mem_unit / kb;
        (*swaptotal) = info.totalswap * info.mem_unit / kb;
        (*swapfree) = info.freeswap * info.mem_unit / kb;
#endif
    }

    /* disk statistics */ {
        uintmax_t disk_in, disk_out;

#ifdef __illumos__
        zpool_iostat_t zpiostat;

        /**
         * ZFS is one of the killer features of Illumos-based Operation System, and
         * the swapfs is also under control by zfs. TODO: In the cloud hosts,
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

#ifdef __macosx__
        struct statfs *mntable;
        io_registry_entry_t dthis, driver;
        io_iterator_t drivestats;
        int64_t value, mntsize, mntidx;
        CFDictionaryRef properties, statistics;
        CFMutableDictionaryRef iomedia;
        CFNumberRef key;

        mntsize = getmntinfo(&mntable, MNT_NOWAIT); /* do not free mntable, it is in static space */
        if (mntsize == 0) goto job_done;

        for (mntidx = 0; mntidx < mntsize; mntidx ++) {
            if (strncmp(mntable[mntidx].f_fstypename, "mtmfs" /* time machine */, 6) == 0) continue;
            if (strncmp(mntable[mntidx].f_fstypename, "devfs", 6) == 0) continue;
            if (strncmp(mntable[mntidx].f_fstypename, "autofs", 7) == 0) continue;

            (*fstotal) += mntable[mntidx].f_blocks * mntable[mntidx].f_bsize / kb;
            (*fsfree) += mntable[mntidx].f_bfree * mntable[mntidx].f_bsize / kb;
        } 

        iomedia = IOServiceMatching(kIOMediaClass);
        CFDictionaryAddValue(iomedia, CFSTR(kIOMediaWholeKey), kCFBooleanTrue);
        errno = IOServiceGetMatchingServices(iostat, iomedia, &drivestats);
        if (errno != KERN_SUCCESS) goto job_done;

        /* see `iostat`.c */
        while ((dthis = IOIteratorNext(drivestats)) != 0 /* not NULL */) {
            errno = IORegistryEntryGetParentEntry(dthis, kIOServicePlane, &driver);
            if (errno != KERN_SUCCESS) goto job_next;
            if (!(IOObjectConformsTo(driver, "IOBlockStorageDriver"))) goto job_skip;

            errno = IORegistryEntryCreateCFProperties(driver, (CFMutableDictionaryRef *)&properties, kCFAllocatorDefault, kNilOptions);
            if (errno != KERN_SUCCESS) goto job_skip;

            statistics = CFDictionaryGetValue(properties, CFSTR(kIOBlockStorageDriverStatisticsKey));
            if (statistics == NULL) goto job_skip;
                                
            key = (CFNumberRef)CFDictionaryGetValue(statistics, CFSTR(kIOBlockStorageDriverStatisticsBytesReadKey));
            if (key != NULL) {
                CFNumberGetValue(key, kCFNumberSInt64Type, &value);
                disk_in += value / kb;
            }

            key = (CFNumberRef)CFDictionaryGetValue(statistics, CFSTR(kIOBlockStorageDriverStatisticsBytesWrittenKey));
            if (key != NULL) {
                CFNumberGetValue(key, kCFNumberSInt64Type, &value);
                disk_out += value / kb;
            }

job_next:
            IOObjectRelease(driver);
job_skip:
            IOObjectRelease(dthis);
        }
        IOObjectRelease(drivestats);

        if (errno != 0) goto job_done;
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
        size_t ifindex /*, ifcount */;
        int ifmib[6];

        sysdatum_size = sizeof(size_t);
        ifmib[0] = CTL_NET;
        ifmib[1] = PF_LINK;
        ifmib[2] = NETLINK_GENERIC;

        /**
         * ifmib[3] = IFMIB_SYSTEM;
         * ifmib[4] = IFMIB_IFCOUNT;
         * status = sysctl(ifmib, 5, &ifcount, &sysdatum_size, NULL, 0);
         * if (status == -1) goto job_done;
         * 
         * This should be the standard way,
         * but weird, the `for` statement does not stop when ifindex < ifcount.
         */

        ifmib[3] = IFMIB_IFDATA;
        ifmib[5] = IFDATA_GENERAL;
        for (ifindex = 1 /* see `man ifmib` */; ifindex /* < ifcount */; ifindex ++) {
            sysdatum_size = sizeof(struct ifmibdata);
            ifmib[4] = ifindex;

            status = sysctl(ifmib, 6, &ifinfo, &sysdatum_size, NULL, 0);
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

