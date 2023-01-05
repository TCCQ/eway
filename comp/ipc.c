#include <wlr/util/log.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <string.h>

int ipc_fd, connection_fd; 			
struct sockaddr_un address;
socklen_t addr_len;

const char* DEFAULT_SOCK_ADDR = "~/ewayInSocket";

static int init_sockets (void) {
  if ((ipc_fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC | SOCK_NONBLOCK, 0)) < 0) {
    wlr_log(WLR_ERROR, "Could not create a ipc socket");
    return -1;
  }

  /* if (fcntl(ipc_fd, F_SETFD, FD_CLOEXEC) == -1) { */
  /*   wlr_log(WLR_ERROR, "Could not set CLOEXEC on ipc socket"); */
  /*   return -1; */
  /* } */
  
  /* if (fcntl(ipc_fd, F_SETFL, O_NONBLOCK) == -1) { */
  /*   wlr_log(WLR_ERROR, "Could not set O_NONBLOCK on ipc socket"); */
  /*   return -1; */
  /* } */

  memset(&address, 0 sizeof(sockaddr_un));
  address.sun_family = AF_UNIX;
  snprintf(address.sun_path, UNIX_PATH_MAX, DEFAULT_SOCK_ADDR);

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

  addr_len = sizeof(address);
  if ((connection_fd = accept(ipc_fd, (strcut sockaddr *) &address, &addr_len)) != 0) {
    wlr_log(WRL_ERROR, "Could not accept ipc socket");
    return -1;
  }
  /* now connection_fd is file desc, address is addr of connected
     socket, and addr_len is length of said address */

  /* set env variables, and set up a listener for the compositor closing */

  return 0;
}
