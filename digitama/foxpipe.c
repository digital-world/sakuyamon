/**
 * This module wraps libssh2 API to work with Racket (sync) and custodian.
 */

#include <scheme.h>
#include <libssh2.h>    /* ld: (ssh2) */

#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/types.h>

/* Hash Types */
const intptr_t HOSTKEY_HASH_MD5  = LIBSSH2_HOSTKEY_HASH_MD5;
const intptr_t HOSTKEY_HASH_SHA1 = LIBSSH2_HOSTKEY_HASH_SHA1;

/* Disconnect Codes (defined by SSH protocol) */
const intptr_t DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT    = SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT;
const intptr_t DISCONNECT_PROTOCOL_ERROR                 = SSH_DISCONNECT_PROTOCOL_ERROR;
const intptr_t DISCONNECT_KEY_EXCHANGE_FAILED            = SSH_DISCONNECT_KEY_EXCHANGE_FAILED;
const intptr_t DISCONNECT_RESERVED                       = SSH_DISCONNECT_RESERVED;
const intptr_t DISCONNECT_MAC_ERROR                      = SSH_DISCONNECT_MAC_ERROR;
const intptr_t DISCONNECT_COMPRESSION_ERROR              = SSH_DISCONNECT_COMPRESSION_ERROR;
const intptr_t DISCONNECT_SERVICE_NOT_AVAILABLE          = SSH_DISCONNECT_SERVICE_NOT_AVAILABLE;
const intptr_t DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED = SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED;
const intptr_t DISCONNECT_HOST_KEY_NOT_VERIFIABLE        = SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE;
const intptr_t DISCONNECT_CONNECTION_LOST                = SSH_DISCONNECT_CONNECTION_LOST;
const intptr_t DISCONNECT_BY_APPLICATION                 = SSH_DISCONNECT_BY_APPLICATION;
const intptr_t DISCONNECT_TOO_MANY_CONNECTIONS           = SSH_DISCONNECT_TOO_MANY_CONNECTIONS;
const intptr_t DISCONNECT_AUTH_CANCELLED_BY_USER         = SSH_DISCONNECT_AUTH_CANCELLED_BY_USER;
const intptr_t DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE = SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE;
const intptr_t DISCONNECT_ILLEGAL_USER_NAME              = SSH_DISCONNECT_ILLEGAL_USER_NAME;

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
    intptr_t clientfd;
} foxpipe_session_t;

typedef struct foxpipe_channel {
    foxpipe_session_t *session;
    LIBSSH2_CHANNEL *channel;
    char read_buffer[2048];
    size_t read_offset;
    size_t read_total;
} foxpipe_channel_t;

static int socket_connect(int clientfd, struct sockaddr *remote, socklen_t addrlen, time_t timeout_ms) {
    socklen_t sockopt_length;
    intptr_t status, origin;

    origin = fcntl(clientfd, F_GETFL, 0);
    fcntl(clientfd, F_SETFL, origin | O_NONBLOCK);
    
    sockopt_length = sizeof(intptr_t);
    getsockopt(clientfd, SOL_SOCKET, SO_ERROR, &status, &sockopt_length);  // Clear the previous error.
    errno = 0;
    status = connect(clientfd, remote, addrlen);
    
    if (errno == EISCONN) {
        errno = 0;
	goto job_done;
    } 
    
    if (errno == EINPROGRESS) {
        struct timespec timeout;
        fd_set pollout;

        MZ_FD_SET(clientfd, &pollout);
        timeout.tv_sec = timeout_ms / 1000;
        timeout.tv_nsec = (timeout_ms % 1000) * 1000 * 1000;

	status = pselect(clientfd + 1, NULL, &pollout, NULL, &timeout, NULL);
	if (status == 0) {
    	    errno = ETIMEDOUT;
	} else if (status == 1) {
            getsockopt(clientfd, SOL_SOCKET, SO_ERROR, &status, &sockopt_length);
            errno = status;
	}

        MZ_FD_CLR(clientfd, &pollout);
    }

job_done:
    return errno;
}

static int socket_shutdown(intptr_t socketfd, int howto) {
    int status;

    errno = 0;
    do { 
      status = shutdown(socketfd, 0);
    } while ((status == -1) && (errno == EINTR));
}

static intptr_t channel_fill_buffer(foxpipe_channel_t *foxpipe) {
    intptr_t read;
    size_t buffer_size;

    buffer_size = sizeof(foxpipe->read_buffer) / sizeof(char);
    read = libssh2_channel_read(foxpipe->channel, foxpipe->read_buffer, buffer_size);

    if (read > 0) {
        foxpipe->read_total = read;
        foxpipe->read_offset = 0;
    }

    return read;
}

static intptr_t channel_close_within_custodian(foxpipe_channel_t *foxpipe) {
    /**
     * The function name just indicates that
     * Racket code is free to leave the channel to the custodian.
     */

    if (foxpipe->read_total >= 0) {
        libssh2_channel_close(foxpipe->channel);
        foxpipe->read_total = -1;
        goto half_done;
    } else if (foxpipe->read_total == -1) {
        libssh2_channel_wait_closed(foxpipe->channel);
        libssh2_channel_free(foxpipe->channel);
        foxpipe->read_total = -2;
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
        rest = foxpipe->read_total - foxpipe->read_offset;

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
        
        src = foxpipe->read_buffer + foxpipe->read_offset;
        delta = (size <= rest) ? size : rest;
        memcpy(dest, src, delta);
        foxpipe->read_offset += delta;
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
    intptr_t read, socketfd;

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
    if (foxpipe->read_total >= 0) { /* Port has not been closed */
        if (foxpipe->read_offset < foxpipe->read_total) {
            read = foxpipe->read_total - foxpipe->read_offset;
        } else {
            scheme_fd_to_semaphore(foxpipe->session->clientfd, MZFD_CREATE_READ, 1);
            read = channel_fill_buffer(foxpipe);
        }
    }

    return (read == LIBSSH2_ERROR_EAGAIN) ? 0 : 1;
}

static void channel_read_need_wakeup(Scheme_Input_Port *port, void *fds) {
    foxpipe_channel_t *channel;
    fd_set *fdin, *fderr;
    intptr_t socketfd;

    /**
     * Called when the port is blocked on a read;
     *
     * It should set appropriate bits in fds to specify which file descriptor(s) it is blocked on.
     * The fds argument is conceptually an array of three fd_set structs (for read, write, and exceptions),
     * but manipulate this array using scheme_get_fdset to get a particular element of the array,
     * and use MZ_FD_XXX instead of FD_XXX to manipulate a single “fd_set”.
     */

    channel = (foxpipe_channel_t *)port->port_data;
    socketfd = channel->session->clientfd;
    
    fdin = ((fd_set *)fds) + 0;
    fderr = ((fd_set *)fds) + 2;

    MZ_FD_SET(socketfd, fdin);
    MZ_FD_SET(socketfd, fderr);
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
    intptr_t socketfd;

    /**
     * Called when the port is blocked on a write;
     *
     * It should set appropriate bits in fds to specify which file descriptor(s) it is blocked on.
     * The fds argument is conceptually an array of three fd_set structs (for read, write, and exceptions),
     * but manipulate this array using scheme_get_fdset to get a particular element of the array,
     * and use MZ_FD_XXX instead of FD_XXX to manipulate a single “fd_set”.
     */

    channel = (foxpipe_channel_t *)port->port_data;
    socketfd = channel->session->clientfd;
    
    fdout = ((fd_set *)fds) + 1;
    fderr = ((fd_set *)fds) + 2;

    MZ_FD_SET(socketfd, fdout);
    MZ_FD_SET(socketfd, fderr);
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

foxpipe_session_t *foxpipe_construct(const char *sshd_host, short sshd_port, time_t timeout_ms, intptr_t *sys_errno, intptr_t *gai_errcode) {
    foxpipe_session_t *session;
    intptr_t clientfd, status;
    struct addrinfo *sshd_info, hints;
    socklen_t addrlen;
    char sshd_service[6];

    scheme_security_check_network("foxpipe_construct", sshd_host, sshd_port, 1 /* client? */);
    snprintf(sshd_service, 6, "%d", sshd_port);
    
    session = NULL;
    clientfd = 0;
    (*sys_errno) = 0;
    (*gai_errcode) = 0;
    memset((void *)&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP; 
    hints.ai_flags = AI_PASSIVE | AI_CANONNAME | AI_ADDRCONFIG;
    
    status = getaddrinfo(sshd_host, sshd_service, &hints, &sshd_info);
    if (status != 0) {
        if (gai_errcode != NULL) {
            (*gai_errcode) = status;
            if (status == EAI_SYSTEM) {
                (*sys_errno) = errno;
            }
        }
        goto job_failed;
    }

    clientfd = socket(sshd_info->ai_family, sshd_info->ai_socktype, sshd_info->ai_protocol);
    if (clientfd > 0) {
        status = socket_connect(clientfd, sshd_info->ai_addr, sshd_info->ai_addrlen, timeout_ms);
        if (status == 0) {
            LIBSSH2_SESSION *sshclient;
    
            /* The racket GC cannot work with libssh2 whose structures' shape is unknown. */
            sshclient = libssh2_session_init();
            if (sshclient != NULL) {
                session = (foxpipe_session_t *)scheme_malloc_atomic(sizeof(foxpipe_session_t));
                session->sshclient = sshclient;
                session->clientfd = clientfd;
                libssh2_session_set_timeout(sshclient, timeout_ms);
                goto job_done;
            }
        }
    }

    (*sys_errno) = errno;

job_failed:
    if (clientfd > 0) {
        socket_shutdown(clientfd, SHUT_RDWR);
    }

job_done:
    if (sshd_info != NULL) {
        freeaddrinfo(sshd_info);
    }

    return session;
}

const char *foxpipe_handshake(foxpipe_session_t *session, intptr_t MD5_or_SHA1) {
    const char *figureprint;
    intptr_t status;

    figureprint = NULL;
    
    status = libssh2_session_handshake(session->sshclient, session->clientfd);
    if (status == 0) {
        figureprint = libssh2_hostkey_hash(session->sshclient, MD5_or_SHA1);
    }

    return figureprint;
}

intptr_t foxpipe_authenticate(foxpipe_session_t *session, const char *wargrey, const char *rsa_pub, const char *id_rsa, const char *passphrase) {
    /* TODO: Authorize based on userauth_list */
    return libssh2_userauth_publickey_fromfile(session->sshclient, wargrey, rsa_pub, id_rsa, passphrase);
}

intptr_t foxpipe_collapse(foxpipe_session_t *session, intptr_t reason_code, const char *description) {
    size_t libssh2_longest_reason_size;
    char *reason;

    if (session->clientfd > 0) {
        libssh2_longest_reason_size = 256;
        reason = (char *)description;
        
        if (strlen(description) > libssh2_longest_reason_size) {
            reason = (char *)scheme_malloc_atomic(sizeof(char) * (libssh2_longest_reason_size + 1));
            strncpy(reason, description, libssh2_longest_reason_size);
            reason[libssh2_longest_reason_size] = '\0';
        }

        libssh2_session_disconnect_ex(session->sshclient, reason_code, reason, "");
        libssh2_session_free(session->sshclient);
        socket_shutdown(session->clientfd, SHUT_RDWR);
        session->clientfd = 0;
    }

    return 0;
}

intptr_t foxpipe_last_errno(foxpipe_session_t *session) {
    return libssh2_session_last_errno(session->sshclient);
}

intptr_t foxpipe_last_error(foxpipe_session_t *session, char **errmsg, intptr_t *size) {
    return libssh2_session_last_error(session->sshclient, errmsg, (int *)size, 0);
}

intptr_t foxpipe_direct_channel(foxpipe_session_t *session,
                                const char* host_seen_by_sshd, intptr_t service,
                                Scheme_Object **sshin, Scheme_Object **sshout) {
    LIBSSH2_CHANNEL *channel;

    /* session timeout has no effects on this */
    libssh2_session_set_blocking(session->sshclient, 1);
    channel = libssh2_channel_direct_tcpip_ex(session->sshclient, host_seen_by_sshd, service, host_seen_by_sshd, 22);
    libssh2_session_set_blocking(session->sshclient, 0);

    if (channel != NULL) {
        foxpipe_channel_t *object;

        object = (foxpipe_channel_t*)scheme_malloc_atomic(sizeof(foxpipe_channel_t));
        object->session = session;
        object->channel = channel;
        object->read_offset = 0;
        object->read_total = 0;

        (*sshin) = (Scheme_Object *)scheme_make_input_port(scheme_make_port_type("<libssh2-channel-input-port>"),
                                                            (void *)object, /* input port data object */
                                                            scheme_intern_symbol("/dev/ssh/chin"), /* (object-name) */
                                                            channel_read_bytes, /* (read-bytes) */
                                                            NULL, /* (peek-bytes): NULL means use the default */
                                                            scheme_progress_evt_via_get, /* (port-progress-evt) */
                                                            scheme_peeked_read_via_get, /* (port-commit-peeked) */
                                                            (Scheme_In_Ready_Fun)channel_read_ready, /* (poll POLLIN) */
                                                            channel_in_close, /* (close_output_port) */
                                                            (Scheme_Need_Wakeup_Input_Fun)channel_read_need_wakeup,
                                                            1 /* lifecycle is managed by custodian */);

        (*sshout) = (Scheme_Object *)scheme_make_output_port(scheme_make_port_type("<libssh2-channel-output-port>"),
                                                            (void *)object, /* output port data object */
                                                            scheme_intern_symbol("/dev/ssh/chout"), /* (object-name) */
                                                            scheme_write_evt_via_write, /* (write-bytes-avail-evt) */
                                                            channel_write_bytes, /* (write-bytes) */
                                                            (Scheme_Out_Ready_Fun)channel_write_ready, /* (poll POLLOUT) */
                                                            channel_out_close, /* (close-output-port) */
                                                            (Scheme_Need_Wakeup_Output_Fun)channel_write_need_wakeup,
                                                            NULL, /* (write-special-evt) */
                                                            NULL, /* (write-special) */
                                                            1 /* lifecycle is managed by custodian */);
    }

    return foxpipe_last_errno(session);
}

