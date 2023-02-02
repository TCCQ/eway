#define _POSIX_C_SOURCE 200112L

#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include <wayland-server-core.h>
#include <wlr/util/log.h>

#include "server.h"
#include "ipc.h"


/* this is ugly and imo bad practice, but I don't really see an
   alternative given the scoping nature of event callbacks. */
struct server *connected_server;


int ipc_fd, connection_fd = -1;
struct wl_event_source *client_event_source = NULL;

#define ipc_read_buffer_length 1024
#define ipc_write_buffer_length 1024
char client_read_buffer[ipc_read_buffer_length];
char client_write_buffer[ipc_write_buffer_length];
int client_read_start = 0;
int client_read_end = 0;
int client_write_start = 0;
int client_write_end = 0;
/* the buffers are circular queues. start is the first char to
   consider as data, and the end is the first char after the data */

struct sockaddr_un address;
socklen_t addr_len;
struct wl_listener ipc_display_destroy;
struct wl_event_source *ipc_event_source = NULL;
const char* DEFAULT_SOCK_ADDR = "/home/tmu/ewaySock";

/* quick forward def */
void ipc_client_disconnect(void);

int ipc_parse(char* line) {
  /* line is null terminated and does not have a \n at the
     end. Attempt to match it with one of the expected requests on the
     ipc socket and perfom said request. Will do a single pass and
     consume tokens along the way. */
  wlr_log(WLR_DEBUG, "GOT IPC: %s", line);
  enum socket_request type;
  int id;
  int args[4];

  int token_number = 0;
  int last_space = -1;
  int idx = 0;
  long int tmp;
  while (line[idx] != 0) {
    while (line[idx] != 0 && line[idx] != ' ') {
      idx++;
    }
    /* found a space or end of line*/
    line[idx] = 0; 		/* allow strcmp */
    char* start = line + last_space + 1;
    if (token_number == 0) {
      /* determine request type */
      if (!strcmp(start, "REBOX")) {
	type = REBOX;
      } else if (!strcmp(start, "CLOSE")) {
	type = CLOSE;
      } else if (!strcmp(start, "HIDE")) {
	type = HIDE;
      } else if (!strcmp(start, "RELEASE")) {
	type = RELEASE;
      } else if (!strcmp(start, "QUIT")) {
	type = QUIT;
      } else {
	wlr_log(WLR_ERROR, "Unknown IPC request: %S", start);
	return -1;
      }
    } else if (token_number == 1 && type != QUIT){
      tmp = atoi(start);
      if (tmp == 0) {
	wlr_log(WLR_ERROR, "ipc request reported id zero. Could it be malformed?");
	return -1;
      }
      id = (int) tmp;
    } else {
      args[token_number - 2] = atoi(start);
    }
    last_space = idx;
    token_number++;
    idx++;
  }

  /* should be all parsed now */
  switch (type) {
  case REBOX:
    resize_translate_view(id, args[0], args[1], args[2], args[3]);
    break;
  case CLOSE:
    close_view(id);
    break;
  case HIDE:
    hide_view(id);
    break;
  case RELEASE:
    focus_view(id);
    break;
  case QUIT:
    ipc_client_disconnect();
    wl_display_terminate(connected_server->display);
    break;
  default:
    break;
  }
}

int ipc_attempt_parse () {
  /* check if the contents of the read line have a compelete request
     as a prefix, and if so handle it */
  int idx = 0;
  int valid_len = (client_read_start > client_read_end)?
    ((ipc_read_buffer_length - client_read_start) + client_read_end) : (client_read_end - client_read_start);
  while (idx < valid_len &&
	 client_read_buffer[(client_read_start + idx) % ipc_read_buffer_length] != '\n') idx++;
  
  if (idx == valid_len) {
    /* line isn't finished */
    /* should this be end - start instead? */
    return 0;
  }

  /* check what line it is, and remove it from the read line */
  char* to_parse = malloc(idx+1); /* replace the \n with a null */
  if (idx > ipc_read_buffer_length - client_read_start) {
    /* split line */
    int first = ipc_read_buffer_length - client_read_start;
    memcpy(to_parse, client_read_buffer + client_read_start, first);
    memcpy(to_parse + first, client_read_buffer, idx + 1 - first);
    to_parse[idx] = 0;
  } else {
    memcpy(to_parse, client_read_buffer + client_read_start, idx+1);
    to_parse[idx] = 0;
  }

  /* update the ends of the circular queue */
  client_read_start = (client_read_start + idx + 1) % ipc_read_buffer_length;

  /* string is linear and has a null terminator, proceed to parsing */
  int ret_val = ipc_parse(to_parse);
  free(to_parse);
  return (ret_val)? ret_val : ipc_attempt_parse();
  /* stop if there is an error, otherwise recurse to catch multiple requests in one readable event */
}

int ipc_queue_write(char* to_write, int len) {
  if (connection_fd == -1) {
    wlr_log(WLR_ERROR, "Can't queue writes, no connection is open");
    return -1;
  }

  ssize_t written;
  do {
    written = write(connection_fd, to_write, len);
  } while (written == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
  if (written == -1) {
    wlr_log(WLR_ERROR, "Could not write to ipc socket");
    return -1;
  }
  return 0;

  /*
   * if ((client_write_end - client_write_start) + len > ipc_write_buffer_length) {
   *   wlr_log(WLR_ERROR, "Queuing write would overflow buffer");
   *   return -1;
   * }
   * 
   * int i = client_write_end;
   * for (int n = 0; n < len; n++) {
   *   client_write_buffer[(i + n) % ipc_write_buffer_length] = to_write[n];
   * }
   * client_write_end = (client_write_end + len) % ipc_write_buffer_length;
   * return 0;
   */
}

void ipc_on_display_destroy (struct wl_listener *listener, void *data) {
  if (ipc_event_source) {
    wl_event_source_remove(ipc_event_source);
  }
  if (connection_fd != -1) {
    close(connection_fd);		/* make sure this is the right fd to close */
    wl_event_source_remove(client_event_source);
  }
  unlink(address.sun_path);
  /* possible memory leak? below causes a segfault when included, but I am not sure why sway includes it at all */
  /*
   * wl_list_remove(&ipc_display_destroy.link);
   */
}

void ipc_client_disconnect(void) {
  if (connection_fd != -1) {
    close(connection_fd);
    connection_fd = -1;    
  }
  if (client_event_source) {
    wl_event_source_remove(client_event_source);
  }
  wlr_log(WLR_DEBUG, "ipc client disconnected");
}

int ipc_client_handle_readable (int client_fd, uint32_t mask, void *data) {
  int read_available;
  if (ioctl(client_fd, FIONREAD, &read_available) == -1) {
    wlr_log(WLR_ERROR, "Unable to read IPC socket buffer size");
    ipc_client_disconnect();
    return -1;
  }

  char buf[ipc_read_buffer_length];
  if (read_available) {
    int len = recv(connection_fd, buf, 1024, 0);
    if (len + (client_read_end - client_read_start) > ipc_read_buffer_length) {
      wlr_log(WLR_ERROR, "IPC read line too long.");
      return -1;
    } else {
      if (len + client_read_end > ipc_read_buffer_length) {
	/* requires wrapparound */
	int first_len = ipc_read_buffer_length - client_read_end;
	memcpy(client_read_buffer + client_read_end, buf, first_len);
	memcpy(client_read_buffer, buf+first_len, len - first_len);
	client_read_end = len-first_len;
      } else {
	memcpy(client_read_buffer + client_read_end, buf, len);
	client_read_end = client_read_end + len;
      }

      /* now new stuff is in read line. Check if we can parse any of it yet */
      return ipc_attempt_parse();
    }
  } else {
    /* nothing to read */
    return 0;
  }
}

/*
 * int ipc_client_handle_writable (int client_fd, uint32_t mask, void *data) {
 *   if (client_write_start == client_write_end) {
 *     /\* nothing to write *\/
 *     return 0;
 *   } else if (client_write_start > client_write_end) {
 *     /\* requires a loop, split into two writes*\/
 *     ssize_t written = write(client_fd, client_write_buffer + client_write_start,
 * 			    ipc_write_buffer_length - client_write_start);
 *     if (written == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
 *       /\* would block, give up *\/
 *       return 0;
 *     } else if (written == -1) {
 *       /\* other error *\/
 *       wlr_log(WLR_ERROR, "Could not write to ipc socket");
 *       return -1;
 *     }
 *       
 *     client_write_start = (client_write_start + written) % ipc_write_buffer_length;
 *     if (client_write_start != 0) {
 *       /\* didn't completely finish first write, not safe to start
 * 	 second currently. leave the remains for the next time *\/
 *       return 0;
 *     }
 *       
 *     /\* do the second half starting from the beginning of the queue *\/
 *     written = write(client_fd, client_write_buffer, client_write_end);
 *     if (written == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
 *       /\* would block, give up *\/
 *       return 0;
 *     } else if (written == -1) {
 *       /\* other error *\/
 *       wlr_log(WLR_ERROR, "Could not write to ipc socket");
 *       return -1;
 *     }
 * 
 *     client_write_start = written;
 *     return 0;
 *   }
 * 
 * 
 *   ssize_t written = write(client_fd, client_write_buffer + client_write_start, client_write_end - client_write_start);
 *   if (written == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
 *     /\* would block, ignore and retry later *\/
 *     return 0;
 *   } else if (written == -1) {
 *     wlr_log(WLR_ERROR, "Could not write to ipc socket");
 *     return -1;
 *   }
 * 
 *   client_write_start = (client_write_start + written) % ipc_write_buffer_length;
 *   
 *   
 * }
 */

int ipc_client_handle_event(int client_fd, uint32_t mask, void *data) {
  if (mask & WL_EVENT_ERROR) {
    wlr_log(WLR_ERROR, "ipc client error");
    return -1;
  }

  if (mask & WL_EVENT_HANGUP) {
    ipc_client_disconnect();
    return 0;
  }

  if (mask & WL_EVENT_READABLE) {
    return ipc_client_handle_readable(client_fd, mask, data);
  }
  /*
   * else if (mask & WL_EVENT_WRITABLE) {
   *   return ipc_client_handle_writable(client_fd, mask, data);
   * }
   */
  return 0;
}

/* creates a new connection on the ipc_fd socket. Returns 0 on success
   and populates connection_fd and client_event_source */
int ipc_handle_connection (int fd, uint32_t mask, void *data) {
  struct server *server = data;
  assert(mask == WL_EVENT_READABLE);

  int client_fd = accept(ipc_fd, NULL, NULL);
  if (client_fd == -1) {
    wlr_log(WLR_ERROR, "Could not accept ipc connection");
    return -1;
  }

  int flags;
  if ((flags = fcntl(client_fd, F_GETFD)) == -1
      || fcntl(client_fd, F_SETFD, flags | FD_CLOEXEC | O_NONBLOCK) == -1) {
    wlr_log(WLR_ERROR, "Could not set flags on client connection");
    return -1;
  }
  
  connection_fd = client_fd;
  client_event_source = wl_event_loop_add_fd(server->wl_event_loop, client_fd, WL_EVENT_READABLE,
					     /* | WL_EVENT_WRITABLE, */
					     ipc_client_handle_event, NULL);
  /* callback for input from the other side of the socket. currently passes no user data */
  return 0;
}

int init_socket (struct server* server) {

  /* save me time when debugging */
  unlink(DEFAULT_SOCK_ADDR);
  
  if ((ipc_fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC | SOCK_NONBLOCK, 0)) < 0) {
    wlr_log(WLR_ERROR, "Could not create a ipc socket");
    return -1;
  }

  memset(&address, 0, sizeof(struct sockaddr_un));
  address.sun_family = AF_UNIX;
  strcpy(address.sun_path, DEFAULT_SOCK_ADDR);
  /*
   * snprintf(address.sun_path, UNIX_PATH_MAX, DEFAULT_SOCK_ADDR);
   */

  if (bind(ipc_fd, (struct sockaddr *) &address, sizeof(struct sockaddr_un)) != 0) {
    wlr_log(WLR_ERROR, "Could not bind ipc socket");
    return -1;
  }

  if (listen(ipc_fd, 5) != 0) {
    wlr_log(WLR_ERROR, "Could not listen on ipc socket");
    return -1;
  }

  /* consider starting the other side here so we know that the socket
     is set up but without blocking for the other side yet */


  if (setenv("EWAYSOCK", DEFAULT_SOCK_ADDR, 1)) {
    wlr_log(WLR_ERROR, "Could not set enviromental variable");
    return -1;
  }

  /*
   * ipc_display_destroy.notify = ipc_on_display_destroy;
   * wl_display_add_destroy_listener(server->display, &ipc_display_destroy);
   */
  ipc_event_source = wl_event_loop_add_fd(server->wl_event_loop, ipc_fd, WL_EVENT_READABLE | WL_EVENT_WRITABLE, ipc_handle_connection, server);


  connected_server = server;
  /* socket is setup. ipc_handle_connection will be automatically
     called when an accepted conenction is made. One should check
     that client_fd is not -1 before trying to do anything with it,
     but the automatic callbacks should handle that */
    
  return 0;
}

int ipc_inform_create (int id) {
  /* inform the other side of the pipe that a new surface has been created */
  if (id == 0) return 0;
  char msg[256];
  int len = snprintf(msg, 256, "NEW %d\n", id);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in the ipc_inform_create call");
    return -1;
  } 
  return ipc_queue_write(msg, len);
}

int ipc_inform_title (int id, const char* title) {
  /* inform the other side that someone changed their title */
  if (id == 0) return 0;
  char msg[256];
  int len = snprintf(msg, 256, "TITLE %d %s\n", id, title);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in the ipc_inform_title call");
    return -1;
  } 
  return ipc_queue_write(msg, len);  
}

int ipc_inform_app_id (int id, const char* app_id) {
  /* inform the other side that someone changed their app_id */
  char msg[256];
  int len = snprintf(msg, 256, "APPID %d %s\n", id, app_id);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in the ipc_inform_app_id call");
    return -1;
  } 
  return ipc_queue_write(msg, len);  
}

int ipc_inform_map (int id) {
  /* inform the other side of the pipe that a surface wants to be mapped */
  if (id == 0) return 0;
  char msg[256];
  int len = snprintf(msg, 256, "MAP %d\n", id);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in the ipc_inform_map call");
    return -1;
  } 
  return ipc_queue_write(msg, len);
}

int ipc_inform_unmap (int id) {
  /* inform the other side of the pipe that a surface wants to be unmapped */
  if (id == 0) return 0;
  char msg[256];
  int len = snprintf(msg, 256, "UNMAP %d\n", id);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in the ipc_inform_unmap call");
    return -1;
  } 
  return ipc_queue_write(msg, len);
}

int ipc_inform_destroy (int id) {
  /* inform the other side of the pipe that the surface with said id should no longer be considered extant */
  if (id == 0) return 0;
  char msg[256];
  int len = snprintf(msg, 256, "DESTROY %d\n", id);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in ipc_inform_destory call");
    return -1;
  }
  return ipc_queue_write(msg, len);
}

int ipc_request_focus(int id) {
  /* request that focus go to the indicated id */
  char msg[256];
  int len = snprintf(msg, 256, "FOCUS %d\n", id);
  if (len < 0) {
    wlr_log(WLR_ERROR, "Error in ipc_request_focus call");
    return -1;
  }
  return ipc_queue_write(msg, len);  
}
