#include "server.h"
/* what requests do we recognize coming from the emacs side of the
   socket? They are numbered here so we can swtich on them, but they
   should share the *exact* name as the string that should lead said
   request */
enum socket_request {
  REBOX,
  CLOSE,
  HIDE,
  RELEASE,
  QUIT
};

int init_socket (struct server* server);

int ipc_queue_write(char* to_write, int len);

int ipc_inform_create(int id);

int ipc_inform_destroy(int id);

int ipc_inform_title(int id, const char* title);

int ipc_inform_app_id(int id, const char* app_id);

int ipc_request_focus(int id);
