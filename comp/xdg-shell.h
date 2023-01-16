#include "server.h"
/* externally visible stuff for xdg_shell and xdg_shell_surface */

struct xdg_decoration {
  struct wlr_xdg_toplevel_decoration_v1 *wlr_decoration;
  struct server *server;
  struct wl_listener destroy;
  struct wl_listener request_mode;
};

int server_init_xdg_shell (struct server* server);
