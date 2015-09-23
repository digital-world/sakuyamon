/**
 * This module wraps libssh2 API to work with Racket (sync) and custodian.
 */

#include <scheme.h>
#include <signal.h>
#include <syslog.h>
#include <setjmp.h>
#include <libssh2.h> /* ld: (ssh2) */

static sigjmp_buf caught_signal;
static void restore_from_signal(int signo, siginfo_t *siginfo, void *whocares) {
    openlog("izuna", LOG_PID | LOG_CONS, LOG_KERN);
    setlogmask(LOG_UPTO(LOG_DEBUG));
    syslog(LOG_WARNING, "caught an unexpected signal: %d[%s]\n", signo, strsignal(signo));
    closelog();
    siglongjmp(caught_signal, signo);
}

/* Hash Types */
intptr_t HOSTKEY_HASH_MD5  = LIBSSH2_HOSTKEY_HASH_MD5;
intptr_t HOSTKEY_HASH_SHA1 = LIBSSH2_HOSTKEY_HASH_SHA1;

/* Disconnect Codes (defined by SSH protocol) */
intptr_t DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT    = SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT;
intptr_t DISCONNECT_PROTOCOL_ERROR                 = SSH_DISCONNECT_PROTOCOL_ERROR;
intptr_t DISCONNECT_KEY_EXCHANGE_FAILED            = SSH_DISCONNECT_KEY_EXCHANGE_FAILED;
intptr_t DISCONNECT_RESERVED                       = SSH_DISCONNECT_RESERVED;
intptr_t DISCONNECT_MAC_ERROR                      = SSH_DISCONNECT_MAC_ERROR;
intptr_t DISCONNECT_COMPRESSION_ERROR              = SSH_DISCONNECT_COMPRESSION_ERROR;
intptr_t DISCONNECT_SERVICE_NOT_AVAILABLE          = SSH_DISCONNECT_SERVICE_NOT_AVAILABLE;
intptr_t DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED = SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED;
intptr_t DISCONNECT_HOST_KEY_NOT_VERIFIABLE        = SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE;
intptr_t DISCONNECT_CONNECTION_LOST                = SSH_DISCONNECT_CONNECTION_LOST;
intptr_t DISCONNECT_BY_APPLICATION                 = SSH_DISCONNECT_BY_APPLICATION;
intptr_t DISCONNECT_TOO_MANY_CONNECTIONS           = SSH_DISCONNECT_TOO_MANY_CONNECTIONS;
intptr_t DISCONNECT_AUTH_CANCELLED_BY_USER         = SSH_DISCONNECT_AUTH_CANCELLED_BY_USER;
intptr_t DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE = SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE;
intptr_t DISCONNECT_ILLEGAL_USER_NAME              = SSH_DISCONNECT_ILLEGAL_USER_NAME;

/**
 * IMPORTANT: to cooperate with the Racket 3m GC, there are three typical SCHEME_MALLOC()s.
 *  1) scheme_malloc(), it's an array of collectable pointers, structure fields must be all pointers;
 *  2) scheme_malloc_atomic(), it's an array of data, structure fields should not contains pointers.
 *  3) scheme_malloc_tagged(), this is the mixture way, a new racket type should be made first.
 * Hence there is no way to pass any of them as the allocator to libssh2_session_init_ex();
 * TODO: should I just make two Racket Types for LIBSSH2_SESSION and LIBSSH2_CHANNEL?
 * TODO: check whether these objects will slow down the GC if they are SCHEME_MALLOC_allow_interior()ed.
 *
 * malloc()ed pointers should be registered with scheme_register_static() or MZ_GC_DECL_REG().
 **/
typedef struct foxpipe_session {
    LIBSSH2_SESSION *sshclient;
    Scheme_Input_Port *dev_tcpin;
    Scheme_Output_Port *dev_tcpout;
} foxpipe_session_t;

static size_t FOXPIPE_CHANNEL_READ_BUFFER_SIZE = 2048;
typedef struct foxpipe_channel {
    foxpipe_session_t *session;
    LIBSSH2_CHANNEL *channel;
    char *read_buffer;
    size_t *read_offset; /* These two cannot point to read_buffer directly, */
    size_t *read_total;  /* since the storage maybe moved by GC. */
} foxpipe_channel_t;

static intptr_t channel_fill_buffer(foxpipe_channel_t *foxpipe) {
    intptr_t read;

    read = libssh2_channel_read(foxpipe->channel, foxpipe->read_buffer, FOXPIPE_CHANNEL_READ_BUFFER_SIZE);

    if (read > 0) {
        (*foxpipe->read_total) = read;
        (*foxpipe->read_offset) = 0;
    }

    return read;
}

static intptr_t channel_close_within_custodian(foxpipe_channel_t *foxpipe) {
    /**
     * The function name just indicates that
     * Racket code is free to leave the channel to the custodian.
     */

    if ((*foxpipe->read_total) >= 0) {
        libssh2_channel_close(foxpipe->channel);
        (*foxpipe->read_total) = -1;
        goto half_done;
    } else if ((*foxpipe->read_total) == -1) {
        libssh2_channel_wait_closed(foxpipe->channel);
        libssh2_channel_free(foxpipe->channel);
        (*foxpipe->read_total) = -2;
        goto full_done;
    }

half_done:
    return 0;

full_done:
    return 1;
}

static intptr_t channel_read_bytes(Scheme_Input_Port *in, char *buffer,
                                   intptr_t offset, intptr_t size,
                                   int nonblock, Scheme_Object *unless) {
    foxpipe_channel_t *foxpipe;
    intptr_t read, rest, delta;
    char *src, *dest;

    /**
     * TODO:
     * Reads bytes into buffer, starting from offset, up to size bytes.
     * switch nonblock
     *     case 0: it can block indefinitely, and return when at least one byte of data is available.
     *     case 1: do not block.
     *     case 2: unbuffered port should return only bytes previously forced to be buffered; otherwise like case 1.
     *     case -1: it can block, but should enable breaks while blocking.
     *
     * The function should return 0 if no bytes are ready in non-blocking mode.
     * It should return EOF if an end-of-file is reached (and no bytes were read into buffer).
     * Otherwise, the function should return the number of read bytes.
     * The function can raise an exception to report an error.
     *
     * The unless argument will be non-NULL only when nonblock is non-zero
     * (except as noted below), and the port supports progress events.
     *
     * If unless is non-NULL and SCHEME_CDR(unless) is non-NULL,
     * the latter is a progress event specific to the port.
     * It should return SCHEME_UNLESS_READY instead of reading bytes
     * if the event in unless becomes ready before bytes can be read.
     * In particular, it should check the event in unless before taking any action,
     * and after any operation that may allow Racket thread swapping.
     * If the read must block, then it should unblock if the event in unless becomes ready.
     *
     * Furthermore, after any potentially thread-swapping operation,
     * it must call scheme_wait_input_allowed, because another thread may be attempting to commit,
     * and unless_evt must be checked after scheme_wait_input_allowed returns.
     * To block, the port should use scheme_block_until_unless instead of scheme_block_until.
     * Finally, in blocking mode, it must return after immediately reading data, without allowing a Racket thread swap.
     */

    foxpipe = (foxpipe_channel_t *)(in->port_data);
    dest = buffer + offset; /* As deep in RVM here, the boundary is already checked. */
    read = 0;

    if (scheme_unless_ready(unless)) {
        /* This is required in all modes. */
        return SCHEME_UNLESS_READY;
    }

    while (size > 0) {
        rest = (*foxpipe->read_total) - (*foxpipe->read_offset);

        if (rest <= 0) {
            if (libssh2_channel_eof(foxpipe->channel) == 1) {
                goto job_failed;
            } else {
                rest = channel_fill_buffer(foxpipe);
                if (rest <= 0) {
                    goto job_done;
                }
            }
        }
        
        src = foxpipe->read_buffer + (*foxpipe->read_offset);
        delta = (size <= rest) ? size : rest;
        memcpy(dest, src, delta);
        (*foxpipe->read_offset) += delta;
        dest = dest + delta;
        read += delta;
        size -= delta;
    }

job_done:
    return read;

job_failed:
    return EOF;
}

static intptr_t channel_read_ready(Scheme_Input_Port *p) {
    foxpipe_channel_t *foxpipe;
    intptr_t read, sockfd, status;

    /**
     * Returns 1 when a non-blocking (read-bytes) will return bytes or an EOF.
     */

    foxpipe = (foxpipe_channel_t *)(p->port_data);

    /**
     * This implementation is correct since all channels in a session are
     * sharing the same socket discriptor. When Racket is woken up by event,
     * It would likely check like this to see if the coming data belongs
     * to this channel.
     */

    read = 1; /* default status, even if the port has been closed */
    if ((*foxpipe->read_total) >= 0) { /* Port has not been closed */
        if ((*foxpipe->read_offset) < (*foxpipe->read_total)) {
            read = (*foxpipe->read_total) - (*foxpipe->read_offset);
        } else {
            status = scheme_get_port_socket((Scheme_Object *)foxpipe->session->dev_tcpin, &sockfd);
            if (status != 0) {
                scheme_fd_to_semaphore(sockfd, MZFD_CREATE_READ, 1);
                read = channel_fill_buffer(foxpipe);
            }
        }
    }

    return (read == LIBSSH2_ERROR_EAGAIN) ? 0 : 1;
}

static void channel_read_need_wakeup(Scheme_Input_Port *port, void *fds) {
    foxpipe_channel_t *channel;
    fd_set *fdin, *fderr;
    intptr_t status, sockfd;

    /**
     * Called when the port is blocked on a read;
     *
     * It should set appropriate bits in fds to specify which file descriptor(s) it is blocked on.
     * The fds argument is conceptually an array of three fd_set structs (for read, write, and exceptions),
     * but manipulate this array using scheme_get_fdset to get a particular element of the array,
     * and use MZ_FD_XXX instead of FD_XXX to manipulate a single “fd_set”.
     */

    channel = (foxpipe_channel_t *)port->port_data;
    status = scheme_get_port_socket((Scheme_Object *)channel->session->dev_tcpin, &sockfd);
    
    if (status != 0) {
        fdin = ((fd_set *)fds) + 0;
        fderr = ((fd_set *)fds) + 2;

        MZ_FD_SET(sockfd, fdin);
        MZ_FD_SET(sockfd, fderr);
    }
}

static void channel_in_close(Scheme_Input_Port *p) {
    foxpipe_channel_t *channel;

    /**
     * Called to close the port.
     * The port is not considered closed until the function returns.
     */

    channel = ((foxpipe_channel_t *)(p->port_data));
    channel_close_within_custodian(channel);
}

static intptr_t channel_write_bytes(Scheme_Output_Port *out, const char *buffer,
                                    intptr_t offset, intptr_t size,
                                    int rarely_block, int enable_break) {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
    char *offed_buffer;
    intptr_t sent;

    /**
     * TODO:
     * Write bytes from buffer, starting from offset, up to size bytes.
     * switch rarely_block
     *   case 0: it can buffer output, and block indefinitely.
     *   case 1: do not buffer output, and block only nothing can be sent.
     *   case 2: do not buffer output, and never blocking.
     *
     * The function should return the number of bytes from buffer that were written;
     * when rarely_block is non-zero and bytes remain in an internal buffer, it should return -1.
     *
     * If enable_break is true, then it should enable breaks while blocking.
     * The function can raise an exception to report an error.
     */

    session = ((foxpipe_channel_t *)(out->port_data))->session->sshclient;
    channel = ((foxpipe_channel_t *)(out->port_data))->channel;
    offed_buffer = (char *)buffer + offset; /* As deep in RVM here, the boundary is already checked. */
    sent = 0;

    /**
     * libssh2 channel does not have write buffer (channel.c _libssh2_channel_write).
     * libssh2 channel only deals with the first 32k bytes (RFC4253 6.1).
     */
    sent = libssh2_channel_write(channel, offed_buffer, size);

job_done:
    return sent;
}

static intptr_t channel_write_ready(Scheme_Output_Port *p) {
    LIBSSH2_CHANNEL *channel;

    /**
     * Returns 1 when a non-blocking (write-bytes) will write at least one byte
     * or flush at least one byte from the port’s internal buffer.
     */

    /**
     * like (channel_read_ready), this implementation is also correct.
     * again, all channels in a session are sharing the same socket descriptor.
     */

    channel = ((foxpipe_channel_t *)(p->port_data))->channel;
    return (libssh2_channel_window_write(channel) > 0) ? 1 : 0;
}

static void channel_write_need_wakeup(Scheme_Output_Port *port, void *fds) {
    foxpipe_channel_t *channel;
    fd_set *fdout, *fderr;
    intptr_t sockfd, status;

    /**
     * Called when the port is blocked on a write;
     *
     * It should set appropriate bits in fds to specify which file descriptor(s) it is blocked on.
     * The fds argument is conceptually an array of three fd_set structs (for read, write, and exceptions),
     * but manipulate this array using scheme_get_fdset to get a particular element of the array,
     * and use MZ_FD_XXX instead of FD_XXX to manipulate a single “fd_set”.
     */

    channel = (foxpipe_channel_t *)port->port_data;
    status = scheme_get_port_socket((Scheme_Object *)channel->session->dev_tcpout, &sockfd);
    
    if (status != 0) {
        fdout = ((fd_set *)fds) + 1;
        fderr = ((fd_set *)fds) + 2;

        MZ_FD_SET(sockfd, fdout);
        MZ_FD_SET(sockfd, fderr);
    }
}

static void channel_out_close(Scheme_Output_Port *p) {
    foxpipe_channel_t *channel;

    /**
     * Called to close the port.
     * The port is not considered closed until the function returns.
     *
     * This function is allowed to block (usually to flush a buffer)
     * unless scheme_close_should_force_port_closed() returns a non-zero result,
     * in which case the function must return without blocking.
     */

    channel = ((foxpipe_channel_t *)(p->port_data));
    channel_close_within_custodian(channel);
    scheme_close_should_force_port_closed();
}

foxpipe_session_t *foxpipe_construct(Scheme_Object *tcp_connect, Scheme_Object *sshd_host, Scheme_Object *sshd_port) {
    foxpipe_session_t *session;
    Scheme_Object *argv[2];

    session = NULL;
    argv[0] = sshd_host;
    argv[1] = sshd_port;
    scheme_apply_multi(tcp_connect, 2, argv);

    if (scheme_current_thread->ku.multiple.count == 2) {
        LIBSSH2_SESSION *sshclient;
        Scheme_Input_Port *dev_tcpin;
        Scheme_Output_Port *dev_tcpout;

        dev_tcpin = (Scheme_Input_Port *)scheme_current_thread->ku.multiple.array[0];
        dev_tcpout = (Scheme_Output_Port *)scheme_current_thread->ku.multiple.array[1];

        /* The racket GC cannot work with libssh2 whose structures' shape is unknown. */
        sshclient = libssh2_session_init();

        if (sshclient != NULL) {
            session = (foxpipe_session_t *)scheme_malloc(sizeof(foxpipe_session_t));
            session->sshclient = sshclient;
            session->dev_tcpin = dev_tcpin;
            session->dev_tcpout = dev_tcpout;
        } else {
            scheme_close_input_port((Scheme_Object *)dev_tcpin);
            scheme_close_output_port((Scheme_Object *)dev_tcpout);
        }
    }

    return session;
}

const char *foxpipe_handshake(foxpipe_session_t *session, intptr_t MD5_or_SHA1) {
    intptr_t status, sockfd;
    const char *figureprint;

    figureprint = NULL;
    
    status = scheme_get_port_socket((Scheme_Object *)session->dev_tcpout, &sockfd);
    if (status != 0) {
        status = libssh2_session_handshake(session->sshclient, sockfd);
        if (status == 0) {
            figureprint = libssh2_hostkey_hash(session->sshclient, MD5_or_SHA1);
        }
    }

    return figureprint;
}

intptr_t foxpipe_authenticate(foxpipe_session_t *session, const char *wargrey, const char *rsa_pub, const char *id_rsa, const char *passphrase) {
    /* TODO: Authorize based on userauth_list */
    return libssh2_userauth_publickey_fromfile(session->sshclient, wargrey, rsa_pub, id_rsa, passphrase);
}

intptr_t foxpipe_collapse(foxpipe_session_t *session, intptr_t reason_code, const char *description) {
    struct sigaction catch_segfault, saved_action;
    intptr_t status;
    
    /**
     * TODO: Meanwhile I've no idea why libssh2_session_disconnect_ex() causes SIGSEGV,
     *       Lucky this point is not quite important, so that I can just ignore it.
     **/

    sigemptyset(&catch_segfault.sa_mask);
    catch_segfault.sa_flags = SA_RESTART | SA_SIGINFO;
    catch_segfault.sa_sigaction = restore_from_signal;
    sigaction(SIGSEGV, &catch_segfault, &saved_action);

    status = sigsetjmp(caught_signal, 1);
    if (status == 0) {
        size_t libssh2_longest_reason_size;
        char *reason;

        libssh2_longest_reason_size = 256;
        reason = (char *)description;
        if (strlen(description) > libssh2_longest_reason_size) {
            reason = (char *)scheme_malloc_atomic(sizeof(char) * (libssh2_longest_reason_size + 1));
            strncpy(reason, description, libssh2_longest_reason_size);
            reason[libssh2_longest_reason_size] = '\0';
        }

        libssh2_session_disconnect_ex(session->sshclient, reason_code, reason, "");
    }

    libssh2_session_free(session->sshclient);
    scheme_close_input_port((Scheme_Object *)session->dev_tcpin);
    scheme_close_output_port((Scheme_Object *)session->dev_tcpout);

    sigaction(SIGSEGV, &saved_action, NULL);

    return status;
}

intptr_t foxpipe_last_errno(foxpipe_session_t *session) {
    return libssh2_session_last_errno(session->sshclient);
}

intptr_t foxpipe_last_error(foxpipe_session_t *session, char **errmsg, intptr_t *size) {
    return libssh2_session_last_error(session->sshclient, errmsg, (int *)size, 0);
}

intptr_t foxpipe_direct_channel(foxpipe_session_t *session,
                                const char* host_seen_by_sshd, intptr_t service,
                                Scheme_Object **dev_sshin, Scheme_Object **dev_sshout) {
    LIBSSH2_CHANNEL *channel;

    libssh2_session_set_blocking(session->sshclient, 1);
    channel = libssh2_channel_direct_tcpip_ex(session->sshclient, host_seen_by_sshd, service, host_seen_by_sshd, 22);
    libssh2_session_set_blocking(session->sshclient, 0);

    if (channel != NULL) {
        foxpipe_channel_t *object;
        void *make_xform_happy_buffer, *make_xform_happy_offset, *make_xform_happy_total;
        Scheme_Input_Port *make_xform_happy_sshin;
        Scheme_Output_Port *make_xform_happy_sshout;

        object = (foxpipe_channel_t*)scheme_malloc(sizeof(foxpipe_channel_t));
        make_xform_happy_buffer = scheme_malloc_atomic(sizeof(char) * FOXPIPE_CHANNEL_READ_BUFFER_SIZE);
        make_xform_happy_offset = scheme_malloc_atomic(sizeof(size_t));
        make_xform_happy_total = scheme_malloc_atomic(sizeof(size_t));

        object->session = session;
        object->channel = channel;
        object->read_buffer = (char *)make_xform_happy_buffer;
        object->read_offset = (size_t *)make_xform_happy_offset;
        object->read_total = (size_t *)make_xform_happy_total;
        (*object->read_offset) = 0;
        (*object->read_total) = 0;

        make_xform_happy_sshin = scheme_make_input_port(scheme_make_port_type("<libssh2-channel-input-port>"),
                                                        (void *)object, /* input port data object */
                                                        scheme_intern_symbol("/dev/sshin"), /* (object-name) */
                                                        channel_read_bytes, /* (read-bytes) */
                                                        NULL, /* (peek-bytes): NULL means use the default */
                                                        scheme_progress_evt_via_get, /* (port-progress-evt) */
                                                        scheme_peeked_read_via_get, /* (port-commit-peeked) */
                                                        (Scheme_In_Ready_Fun)channel_read_ready, /* (poll POLLIN) */
                                                        channel_in_close, /* (close_output_port) */
                                                        (Scheme_Need_Wakeup_Input_Fun)channel_read_need_wakeup,
                                                        1 /* lifecycle is managed by custodian */);

        make_xform_happy_sshout = scheme_make_output_port(scheme_make_port_type("<libssh2-channel-output-port>"),
                                                            (void *)object, /* output port data object */
                                                            scheme_intern_symbol("/dev/sshout"), /* (object-name) */
                                                            scheme_write_evt_via_write, /* (write-bytes-avail-evt) */
                                                            channel_write_bytes, /* (write-bytes) */
                                                            (Scheme_Out_Ready_Fun)channel_write_ready, /* (poll POLLOUT) */
                                                            channel_out_close, /* (close-output-port) */
                                                            (Scheme_Need_Wakeup_Output_Fun)channel_write_need_wakeup,
                                                            NULL, /* (write-special-evt) */
                                                            NULL, /* (write-special) */
                                                            1 /* lifecycle is managed by custodian */);

        (*dev_sshin) = (Scheme_Object *)make_xform_happy_sshin;
        (*dev_sshout) = (Scheme_Object *)make_xform_happy_sshout;
    }

    return foxpipe_last_errno(session);
}

