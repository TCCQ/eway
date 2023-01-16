#include "server.h"
/* what requests do we recognize coming from the emacs side of the
   socket? They are numbered here so we can swtich on them, but they
   should share the *exact* name as the string that should lead said
   request */
enum socket_request {
  REBOX,
  CLOSE,
  HIDE,
  RELEASE
};

int init_socket (struct server* server);

int ipc_queue_write(char* to_write, int len);

int ipc_inform_create(int id, const char* name);

int ipc_inform_destroy(int id);
