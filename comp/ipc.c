#include <wayland-server-core.h>
#include <wlr/util/log.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include "server.h"
#include "ipc.h"

int ipc_fd, connection_fd = -1;
struct wl_event_source *client_event_source = NULL;
/* consider buffer for unfinished stuff from the client */

struct sockaddr_un address;
socklen_t addr_len;
struct wl_listener ipc_display_destroy;
struct wl_event_source *ipc_event_source = NULL;
const char* DEFAULT_SOCK_ADDR = "/home/tmu/ewayInSocket";

static void ipc_on_display_destroy (struct wl_listener *listener, void *data) {
  if (ipc_event_source) {
    wl_event_source_remove(ipc_event_source);
  }
  close(connection_fd);		/* make sure this is the right fd to close */
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
  if (mask & WL_EVENT_ERROR) {
    wlr_log(WLR_ERROR, "ipc client error");
    return -1;
  }

  if (mask & WL_EVENT_HANGUP) {
    ipc_client_disconnect();
    return 0;
  }

  int read_available;
  if (ioctl(client_fd, FIONREAD, &read_available) == -1) {
    wlr_log(WLR_ERROR, "Unable to read IPC socket buffer size");
    ipc_client_disconnect();
    return -1;
  }

  char buf[1024];
  if (read_available) {
    int len = recv(connection_fd, buf, 1024, 0);
    buf[len] = 0;
    wlr_log(WLR_DEBUG, "%s", buf);
  }
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
  client_event_source = wl_event_loop_add_fd(server->wl_event_loop, client_fd, WL_EVENT_READABLE, ipc_client_handle_readable, NULL);
  /* callback for input from the other side of the socket. currently passes no user data */
  return 0;
}

int init_socket (void) {
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

  struct server *server = global_server; /* server.h */
  ipc_display_destroy.notify = ipc_on_display_destroy;
  wl_display_add_destroy_listener(server->display, &ipc_display_destroy);
  ipc_event_source = wl_event_loop_add_fd(server->wl_event_loop, ipc_fd, WL_EVENT_READABLE, ipc_handle_connection, server);
    
  /* socket is setup. ipc_handle_connection will be automatically
     called when an accepted conenction is made. One should check
     that client_fd is not -1 before trying to do anything with it,
     but the automatic callbacks should handle that */
    
  return 0;
}
