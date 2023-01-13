#include "server.h"

struct keyboard {
  struct wl_list link;
  struct server *server;
  struct wlr_keyboard *wlr_keyboard;

  struct wl_listener modifiers;
  struct wl_listener key;
  struct wl_listener destroy;
};

void keyboard_focus_to_view (struct view* view, struct wlr_surface* surface);

void server_init_input (struct server* server);
