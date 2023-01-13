#include <stdlib.h>
#include <assert.h>

#include <wayland-server-core.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>

#include <wlr/util/log.h>

#include "server.h"
#include "input.h"
#include "ipc.h"
/*
 * xdg_shell stuff. mostly stolen from tinywl
 */

void xdg_toplevel_map (struct wl_listener *listener, void *data) {
  /* surface is mapped (ready to display) */
  struct view *view = wl_container_of(listener, view, map);

  wl_list_insert(&view->server->views, &view->link);

  /* focus? */
  keyboard_focus_to_view(view, view->xdg_toplevel->base->surface);
}

void xdg_toplevel_unmap (struct wl_listener *listener, void *data) {
  struct view *view = wl_container_of(listener, view, unmap);

  wl_list_remove(&view->link);
}

void xdg_toplevel_destroy (struct wl_listener *listener, void *data) {
  struct view *view = wl_container_of(listener, view, destroy);

  wl_list_remove(&view->map.link);
  wl_list_remove(&view->unmap.link);
  wl_list_remove(&view->destroy.link);

  /* inform the other side of the socket about something closing */
  if (ipc_inform_destroy((eway_id_t) view)) {
    wlr_log(WLR_ERROR, "Encountered some problem during inform_destor");
  }

  free(view);
}

void server_new_xdg_surface (struct wl_listener *listener, void *data) {
  /* raised when a client requests a new xdg surface */
  struct server *server = wl_container_of(listener, server, new_xdg_surface);
  struct wlr_xdg_surface *xdg_surface = data;

  /* add popups to scene graph so they can be rendered. They need to have a proper parent, so we set the user data field as said scene node (?) */
  if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
    struct wlr_xdg_surface *parent = wlr_xdg_surface_from_wlr_surface(xdg_surface->popup->parent);
    struct wlr_scene_tree *parent_tree = parent->data;
    xdg_surface->data = wlr_scene_xdg_surface_create(parent_tree, xdg_surface);
    return;
  }

  assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);

  /* allocate a view */
  struct view *view = calloc(1, sizeof(struct view));
  view->server = server;
  view->xdg_toplevel = xdg_surface->toplevel;
  view->scene_tree = wlr_scene_xdg_surface_create(&view->server->scene->tree, view->xdg_toplevel->base);
  view->scene_tree->node.data = view;
  xdg_surface->data = view->scene_tree;

  /* set listeners */
  view->map.notify = xdg_toplevel_map;
  wl_signal_add(&xdg_surface->events.map, &view->map);
  view->unmap.notify = xdg_toplevel_unmap;
  wl_signal_add(&xdg_surface->events.unmap, &view->unmap);
  view->destroy.notify = xdg_toplevel_destroy;
  wl_signal_add(&xdg_surface->events.destroy, &view->destroy);

  /* tell the other side of the socket about this new surface */
  if (ipc_inform_create((eway_id_t) view, "testing")) {
    wlr_log(WLR_ERROR, "encounterd some problem during inform_create");
  }

  /* keyboard_focus_to_view(view, xdg_surface->surface); */
}

void server_init_xdg_shell (struct server* server) {
  server->xdg_shell = wlr_xdg_shell_create(server->display, 3); 
  server->new_xdg_surface.notify = server_new_xdg_surface;
  wl_signal_add(&server->xdg_shell->events.new_surface, &server->new_xdg_surface);  
}
