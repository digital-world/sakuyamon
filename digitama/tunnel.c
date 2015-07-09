/**
 * This file implements the libssh2 channel ports
 * that suitable to work with Racket (sync).
 */

#include <scheme.h>
#include <libssh2.h>

typedef struct port_object {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
} Port_Object;

static void channel_close_within_custodian(LIBSSH2_CHANNEL *channel) {
    /**
     * The function name just indicates that
     * Racket code is free to leave the channel to the custodian.
     */

    libssh2_channel_close(channel);
    libssh2_channel_wait_closed(channel);
    libssh2_channel_free(channel);
}

static intptr_t channel_read_bytes(Scheme_Input_Port *in, char *buffer, intptr_t offset, intptr_t size, int nonblock, Scheme_Object *unless) {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
    intptr_t total, read, status;
    intptr_t saved_blockbit;
    char *offed_buffer;
    
    /**
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

    session = ((struct port_object *)(in->port_data))->session;
    channel = ((struct port_object *)(in->port_data))->channel;
    saved_blockbit = libssh2_session_get_blocking(session);
    offed_buffer = buffer + offset; /* As deep in RVM here, the boundary is already checked. */
    total = size;
    read = 0;

    if (scheme_unless_ready(unless)) {
        /* This is required in all modes. */
        return SCHEME_UNLESS_READY;
    }

    if (libssh2_channel_eof(channel) == 1) {
        char *signal, *errmsg;
        size_t sigsize, msgsize;
        int status, errno;

        status = libssh2_channel_get_exit_status(channel);
        errno = libssh2_channel_get_exit_signal(channel, &signal, &sigsize, &errmsg, &msgsize, NULL, NULL);
        printf("Remote Exit with status %d[%d]: %s; Signal: %s.\n", status, errno, errmsg, signal);

        return EOF;
    }

try_read:
    do {
        size -= status;
        if (size > 0) {
            offed_buffer += status;
            status = libssh2_channel_read(channel, offed_buffer, size);
            read += ((status > 0) ? status : 0);
        }
    } while((nonblock <= 0) && (status < size) && (status >= 0));

    if (status == LIBSSH2_ERROR_EAGAIN) {
        if (nonblock >= 1) {
            goto job_done;
        } else {
            if (nonblock == 0) {
                /* also disable the breaking */
                libssh2_session_set_blocking(session, 1);
            }

            status = 0;
            goto try_read;
        }
    }
job_done:

    libssh2_channel_set_blocking(session, saved_blockbit);

    return ((read == total) || (read > 0)) ? read : status;
}

static int channel_read_ready(Scheme_Input_Port *p) {
    LIBSSH2_CHANNEL *channel;
    LIBSSH2_SESSION *session;
    char this_can_be_null;
    int saved_blockbit, read;

    /**
     * Returns 1 when a non-blocking (read-bytes) will return bytes or an EOF.
     */

    /**
     * TODO: this implementation is ugly.
     * I do not know how to extract the socket descriptor from channel.
     * They hide it.
     */
    session = ((struct port_object *)(p->port_data))->session;
    channel = ((struct port_object *)(p->port_data))->channel;
    saved_blockbit = libssh2_session_get_blocking(session);

    libssh2_session_set_blocking(session, 0);
    read = libssh2_channel_read(channel, &this_can_be_null, 0);
    libssh2_session_set_blocking(session, saved_blockbit);

    return (read == LIBSSH2_ERROR_EAGAIN) ? 0 : 1;
}

static void channel_in_close(Scheme_Input_Port *p) {
    LIBSSH2_CHANNEL *channel;

    /**
     * Called to close the port.
     * The port is not considered closed until the function returns.
     */

    channel = ((struct port_object *)(p->port_data))->channel;
    channel_close_within_custodian(channel);
}

static intptr_t channel_write_bytes(Scheme_Output_Port *out, char *buffer, intptr_t offset, intptr_t size, int rarely_block, int enable_break) {
    LIBSSH2_SESSION *session;
    LIBSSH2_CHANNEL *channel;
    char *offed_buffer;
    int saved_blockbit;
    int status, sent, total;

    /**
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

    session = ((struct port_object *)(out->port_data))->session;
    channel = ((struct port_object *)(out->port_data))->channel;
    saved_blockbit = libssh2_session_get_blocking(session);
    offed_buffer = buffer + offset; /* As deep in RVM here, the boundary is already checked. */
    total = size;
    sent = 0;
    status = 0;

    libssh2_session_set_blocking(session, 0);
try_send:
    do { /**
          * libssh2 channel does not have write buffer (channel.c _libssh2_channel_write).
          * libssh2 channel only deals with the first 32k bytes (RFC4253 6.1).
          */
        size -= status;
        if (size > 0) {
            offed_buffer += status;
            status = libssh2_channel_write(channel, offed_buffer, size);
            sent += ((status > 0) ? status : 0);
        }
    } while((rarely_block == 0) && (size > 32700) && (status == 32700));

    if ((sent == total) || (rarely_block * sent > 0)) {
        /* sent == total == 0 also goes here */
        goto job_done;
    }

    if (status == LIBSSH2_ERROR_EAGAIN) {
        if (rarely_block == 2) {
            goto job_done;
        } else {
            if (enable_break == 0) {
                libssh2_session_set_blocking(session, 1);
            }

            status = 0;
            goto try_send;
        }
    }

job_done:
    libssh2_session_set_blocking(session, saved_blockbit);

    return ((sent == total) || (sent > 0)) ? sent : status;
}

static int channel_write_ready(Scheme_Output_Port *p) {
    LIBSSH2_CHANNEL *channel;

    /**
     * Returns 1 when a non-blocking (write-bytes) will write at least one byte
     * or flush at least one byte from the port’s internal buffer.
     */

    channel = ((struct port_object *)(p->port_data))->channel;
    return (libssh2_channel_window_write(channel) > 0) ? 1 : 0;
}

static void channel_out_close(Scheme_Output_Port *p) {
    LIBSSH2_CHANNEL *channel;

    /**
     * Called to close the port.
     * The port is not considered closed until the function returns.
     *
     * This function is allowed to block (usually to flush a buffer)
     * unless scheme_close_should_force_port_closed returns a non-zero result,
     * in which case the function must return without blocking.
     */

    channel = ((struct port_object *)(p->port_data))->channel;
    channel_close_within_custodian(channel);
}

int open_input_output_direct_channel(LIBSSH2_SESSION* session, const char *gyoudmon, int service, Scheme_Object **read, Scheme_Object **write) {
    LIBSSH2_CHANNEL *channel;
    struct port_object *object;
    int saved_blockbit;

    saved_blockbit = libssh2_session_get_blocking(session);
    libssh2_session_set_blocking(session, 1); /* also disable the breaking */
    channel = libssh2_channel_direct_tcpip_ex(session, gyoudmon, service, "localhost", 22);
    libssh2_session_set_blocking(session, saved_blockbit);

    if (channel != NULL) {
        struct port_object *object = (Port_Object *)scheme_malloc_atomic(sizeof(struct port_object));
        object->session = session;
        object->channel = channel;

        (*read) = (Scheme_Object *)scheme_make_input_port(scheme_make_port_type("<libssh2-channel-input-port>"),
                                                          (void *)object, /* input port data object */
                                                          scheme_intern_symbol("/dev/sshin"), /* (object-name) */
                                                          channel_read_bytes, /* (read-bytes) */
                                                          NULL, /* (peek-bytes): NULL means use the default */
                                                          scheme_progress_evt_via_get, /* (port-progress-evt) */
                                                          scheme_peeked_read_via_get, /* (port-commit-peeked) */
                                                          channel_read_ready, /* (poll POLLIN) */
                                                          channel_in_close, /* (close_output_port) */
                                                          NULL, /* (scheme_need_wakeup) */
                                                          1 /* lifecycle is managed by custodian */);

        (*write) = (Scheme_Object *)scheme_make_output_port(scheme_make_port_type("<libssh2-channel-output-port>"),
                                                            (void *)object, /* output port data object */
                                                            scheme_intern_symbol("/dev/sshout"), /* (object-name) */
                                                            scheme_write_evt_via_write, /* (write-bytes-avail-evt) */
                                                            channel_write_bytes, /* (write-bytes) */
                                                            channel_write_ready, /* (poll POLLOUT) */
                                                            channel_out_close, /* (close-output-port) */
                                                            NULL, /* (scheme_need_wakeup) */
                                                            NULL, /* (write-special-evt) */
                                                            NULL, /* (write-special) */
                                                            1 /* lifecycle is managed by custodian */);
    }

    return libssh2_session_last_errno(session);
}

