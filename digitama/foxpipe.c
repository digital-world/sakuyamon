/**
 * This file implements the libssh2 channel ports
 * that suitable to work with Racket (sync).
 */

#include <scheme.h>
#include <libssh2.h>

typedef struct foxpipe_session {
    LIBSSH2_SESSION *sshclient;
    Scheme_Input_Port *dev_tcpin;
    Scheme_Output_Port *dev_tcpout;
} foxpipe_session_t;

typedef struct foxpipe_channel {
    foxpipe_session_t *session;
    LIBSSH2_CHANNEL *channel;
    char read_ready[1];
} foxpipe_channel_t;

static void channel_close_within_custodian(foxpipe_channel_t *foxpipe) {
    /**
     * The function name just indicates that
     * Racket code is free to leave the channel to the custodian.
     */

    if (foxpipe->read_ready[0] != '\127') {
        libssh2_channel_close(foxpipe->channel);
        foxpipe->read_ready[0] = '\127'; 
    } else {
        libssh2_channel_wait_closed(foxpipe->channel);
        libssh2_channel_free(foxpipe->channel);
    }
}

static intptr_t channel_read_bytes(Scheme_Input_Port *in, char *buffer, intptr_t offset, intptr_t size, int nonblock, Scheme_Object *unless) {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
    intptr_t read, status;
    intptr_t saved_blockbit;
    char *onebuffer, *offed_buffer;

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

    session = ((foxpipe_channel_t *)(in->port_data))->session->sshclient;
    channel = ((foxpipe_channel_t *)(in->port_data))->channel;
    onebuffer = ((foxpipe_channel_t *)(in->port_data))->read_ready;
    saved_blockbit = libssh2_session_get_blocking(session);
    offed_buffer = buffer + offset; /* As deep in RVM here, the boundary is already checked. */
    read = 0;

    if (scheme_unless_ready(unless)) {
        /* This is required in all modes. */
        return SCHEME_UNLESS_READY;
    }

    if (size > 0) {
        if (libssh2_channel_eof(channel) == 1) {
            if ((*onebuffer) != '\0') {
                (*offed_buffer) = (*onebuffer);
                (*onebuffer) = '\0';
                read = 1;
                goto job_done;
            }

            return EOF;
        }

        libssh2_session_set_blocking(session, 0);

        if ((*onebuffer) != '\0') {
            (*offed_buffer) = (*onebuffer);
            (*onebuffer) = '\0';
            offed_buffer += 1;
            read += 1;
            size -= 1;
        }

        status = libssh2_channel_read(channel, offed_buffer, size);
        if (status >= 0) {
            read += status;
        }

        libssh2_session_set_blocking(session, saved_blockbit);
    }

job_done:
    return read;
}

static int channel_read_ready(Scheme_Input_Port *p) {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
    int saved_blockbit, read;
    char *onebuffer;

    /**
     * Returns 1 when a non-blocking (read-bytes) will return bytes or an EOF.
     */

    /**
     * This implementation is correct since all channels in a session are
     * sharing the same socket discriptor. When Racket is woken up by event
     * (say, what does channel_read_need_wakeup prepare for), it would likely
     * check like this to see if the coming data belongs to this channel.
     */

    session = ((foxpipe_channel_t *)(p->port_data))->session->sshclient;
    channel = ((foxpipe_channel_t *)(p->port_data))->channel;
    onebuffer = ((foxpipe_channel_t *)(p->port_data))->read_ready;
    saved_blockbit = libssh2_session_get_blocking(session);

    if ((*onebuffer) == '\0') {
        libssh2_session_set_blocking(session, 0);
        read = libssh2_channel_read(channel, onebuffer, 1);
        libssh2_session_set_blocking(session, saved_blockbit);
    } else {
        read = 1;
    }

    return (read == LIBSSH2_ERROR_EAGAIN) ? 0 : 1;
}

static void channel_read_need_wakeup(Scheme_Input_Port *port, void *fds) {
    foxpipe_channel_t *channel;
    intptr_t dev_tcpin;
    fd_set *fdin, *fderr;

    /**
     * Called when the port is blocked on a read;
     *
     * It should set appropriate bits in fds to specify which file descriptor(s) it is blocked on.
     * The fds argument is conceptually an array of three fd_set structs (for read, write, and exceptions),
     * but manipulate this array using scheme_get_fdset to get a particular element of the array,
     * and use MZ_FD_XXX instead of FD_XXX to manipulate a single “fd_set”.
     */

    channel = (foxpipe_channel_t *)port->port_data;
    fdin = ((fd_set *)fds) + 0;
    fderr = ((fd_set *)fds) + 2;

    scheme_get_port_socket((Scheme_Object *)channel->session->dev_tcpin, &dev_tcpin);
    MZ_FD_SET(dev_tcpin, fdin);
    MZ_FD_SET(dev_tcpin, fderr);
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

static intptr_t channel_write_bytes(Scheme_Output_Port *out, const char *buffer, intptr_t offset, intptr_t size, int rarely_block, int enable_break) {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
    char *offed_buffer;
    int saved_blockbit;
    int sent ;

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
    saved_blockbit = libssh2_session_get_blocking(session);
    offed_buffer = (char *)buffer + offset; /* As deep in RVM here, the boundary is already checked. */
    sent = 0;


    /**
     * libssh2 channel does not have write buffer (channel.c _libssh2_channel_write).
     * libssh2 channel only deals with the first 32k bytes (RFC4253 6.1).
     */
    libssh2_session_set_blocking(session, 0);
    sent = libssh2_channel_write(channel, offed_buffer, size);

job_done:
    libssh2_session_set_blocking(session, saved_blockbit);

    return sent;
}

static int channel_write_ready(Scheme_Output_Port *p) {
    LIBSSH2_CHANNEL *channel;

    /**
     * Returns 1 when a non-blocking (write-bytes) will write at least one byte
     * or flush at least one byte from the port’s internal buffer.
     */

    /**
     * Like (channel_read_ready), this implementation is also correct.
     * Again, all channels in a session are sharing the same socket descriptor.
     */

    channel = ((foxpipe_channel_t *)(p->port_data))->channel;
    return (libssh2_channel_window_write(channel) > 0) ? 1 : 0;
}

static void channel_write_need_wakeup(Scheme_Input_Port *port, void *fds) {
    foxpipe_channel_t *channel;
    intptr_t dev_tcpout;
    fd_set *fdout, *fderr;

    /**
     * Called when the port is blocked on a write;
     *
     * It should set appropriate bits in fds to specify which file descriptor(s) it is blocked on.
     * The fds argument is conceptually an array of three fd_set structs (for read, write, and exceptions),
     * but manipulate this array using scheme_get_fdset to get a particular element of the array,
     * and use MZ_FD_XXX instead of FD_XXX to manipulate a single “fd_set”.
     */

    channel = (foxpipe_channel_t *)port->port_data;
    fdout = ((fd_set *)fds) + 1;
    fderr = ((fd_set *)fds) + 2;

    scheme_get_port_socket((Scheme_Object *)channel->session->dev_tcpout, &dev_tcpout);
    MZ_FD_SET(dev_tcpout, fdout);
    MZ_FD_SET(dev_tcpout, fderr);
}

static void channel_out_close(Scheme_Output_Port *p) {
    foxpipe_channel_t *channel;

    /**
     * Called to close the port.
     * The port is not considered closed until the function returns.
     *
     * This function is allowed to block (usually to flush a buffer)
     * unless scheme_close_should_force_port_closed returns a non-zero result,
     * in which case the function must return without blocking.
     */

    channel = ((foxpipe_channel_t *)(p->port_data));
    channel_close_within_custodian(channel);

    /* The documentation says this function must be called here */
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
        sshclient = libssh2_session_init();

        if (sshclient != NULL) {
            session = (foxpipe_session_t *)scheme_malloc_atomic(sizeof(foxpipe_session_t));
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

const char *foxpipe_handshake(foxpipe_session_t *session, int MD5_or_SHA1) {
    intptr_t sshc_fd, status;
    const char *figureprint;

    figureprint = NULL;
    scheme_get_port_socket((Scheme_Object *)session->dev_tcpin, &sshc_fd);

    status = libssh2_session_handshake(session->sshclient, sshc_fd);

    if (status == 0) {
        figureprint = libssh2_hostkey_hash(session->sshclient, MD5_or_SHA1);
    }

    return figureprint;
}

int foxpipe_authenticate(foxpipe_session_t *session, const char *wargrey, const char *rsa_pub, const char *id_rsa, const char *passphrase) {
    /* TODO: Authorize based on userauth_list */
    return libssh2_userauth_publickey_fromfile(session->sshclient, wargrey, rsa_pub, id_rsa, passphrase);
}

int foxpipe_collapse(foxpipe_session_t *session, int reason, const char *description) {
    /* TODO: It's better to handle the errors */

    libssh2_session_disconnect_ex(session->sshclient, reason, description, "");
    libssh2_session_free(session->sshclient);

    scheme_close_input_port((Scheme_Object *)session->dev_tcpin);
    scheme_close_output_port((Scheme_Object *)session->dev_tcpout);

    return 0;
}

int foxpipe_last_errno(foxpipe_session_t *session) {
    return libssh2_session_last_errno(session->sshclient);
}

int foxpipe_last_error(foxpipe_session_t *session, char **errmsg, int *size) {
    return libssh2_session_last_error(session->sshclient, errmsg, size, 0);
}

int foxpipe_direct_channel(foxpipe_session_t *session, const char* host_seen_by_sshd, int service, Scheme_Object **dev_sshin, Scheme_Object **dev_sshout) {
    LIBSSH2_CHANNEL *channel;
    foxpipe_channel_t *object;
    int saved_blockbit;

    saved_blockbit = libssh2_session_get_blocking(session->sshclient);
    libssh2_session_set_blocking(session->sshclient, 1); /* also disable the breaking */
    channel = libssh2_channel_direct_tcpip_ex(session->sshclient, host_seen_by_sshd, service, host_seen_by_sshd, 22);
    libssh2_session_set_blocking(session->sshclient, saved_blockbit);

    if (channel != NULL) {
        foxpipe_channel_t *object = (foxpipe_channel_t*)scheme_malloc_atomic(sizeof(foxpipe_channel_t));
        object->session = session;
        object->channel = channel;
        object->read_ready[0] = '\0';

        (*dev_sshin) = (Scheme_Object *)scheme_make_input_port(scheme_make_port_type("<libssh2-channel-input-port>"),
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

        (*dev_sshout) = (Scheme_Object *)scheme_make_output_port(scheme_make_port_type("<libssh2-channel-output-port>"),
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
    }

    return foxpipe_last_errno(session);
}

