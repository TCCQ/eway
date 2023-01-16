#include <stdlib.h>
#include <assert.h>

#include <wayland-server-core.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_server_decoration.h>
#include <wlr/types/wlr_scene.h>

#include <wlr/util/log.h>

#include "xdg-shell.h"
#include "server.h"
#include "input.h"
#include "ipc.h"
#include "id.h"
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
  if (ipc_inform_destroy(view->id)) {
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
  view->id = id_allocate(view);
  if (view->id < 0) {
    wlr_log(WLR_ERROR, "Error with id allocation");
    return;
  }

  /* set listeners */
  view->map.notify = xdg_toplevel_map;
  wl_signal_add(&xdg_surface->events.map, &view->map);
  view->unmap.notify = xdg_toplevel_unmap;
  wl_signal_add(&xdg_surface->events.unmap, &view->unmap);
  view->destroy.notify = xdg_toplevel_destroy;
  wl_signal_add(&xdg_surface->events.destroy, &view->destroy);

  /* tell the other side of the socket about this new surface */
  if (ipc_inform_create(view->id, "testing")) {
    wlr_log(WLR_ERROR, "encounterd some problem during inform_create");
  }

  /* keyboard_focus_to_view(view, xdg_surface->surface); */
}

void xdg_decoration_handle_destroy (struct wl_listener *listener, void *data) {
  struct xdg_decoration *xdg_decoration = wl_container_of(listener, xdg_decoration, destroy);

  wl_list_remove(&xdg_decoration->destroy.link);
  wl_list_remove(&xdg_decoration->request_mode.link);
  free(xdg_decoration);
}

void xdg_decoration_enforce_csd_mode(struct xdg_decoration* deco,  bool csd) {
  enum wlr_xdg_toplevel_decoration_v1_mode mode;

  /* we always want the server to deal with it */
  if (csd) {
    mode = WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;    
  } else {
    mode = WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
  }
  wlr_xdg_toplevel_decoration_v1_set_mode(deco->wlr_decoration, mode);  
}

void xdg_decoration_handle_request_mode(struct wl_listener *listener, void *data) {
  struct xdg_decoration *xdg_decoration = wl_container_of(listener, xdg_decoration, destroy);
  enum wlr_xdg_toplevel_decoration_v1_mode mode;

  /* we always want the server to deal with it */
  /*
   * if (xdg_decoration->server->xdg_decoration) {
   */
  mode = WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
  /*
   * } else {
   *   mode = WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;
   * }
   */
  wlr_xdg_toplevel_decoration_v1_set_mode(xdg_decoration->wlr_decoration, mode);
}

void server_new_xdg_toplevel_decoration (struct wl_listener *listener, void *data) {
  struct server *server = wl_container_of(listener, server, xdg_toplevel_decoration);
  struct wlr_xdg_toplevel_decoration_v1 *wlr_decoration = data;

  struct xdg_decoration *xdg_decoration = calloc(1, sizeof(struct xdg_decoration));
  if (!xdg_decoration) {
    return;
  }

  xdg_decoration->wlr_decoration = wlr_decoration;
  xdg_decoration->server = server;

  xdg_decoration->destroy.notify = xdg_decoration_handle_destroy;
  wl_signal_add(&wlr_decoration->events.destroy, &xdg_decoration->destroy);
  xdg_decoration->request_mode.notify = xdg_decoration_handle_request_mode;
  wl_signal_add(&wlr_decoration->events.request_mode, &xdg_decoration->request_mode);

  xdg_decoration_handle_request_mode(&xdg_decoration->request_mode, wlr_decoration);
  xdg_decoration_enforce_csd_mode(xdg_decoration, false);
}

int server_init_xdg_shell (struct server* server) {
  server->xdg_shell = wlr_xdg_shell_create(server->display, 3); 
  server->new_xdg_surface.notify = server_new_xdg_surface;
  wl_signal_add(&server->xdg_shell->events.new_surface, &server->new_xdg_surface);

  server->xdg_decoration_manager = wlr_xdg_decoration_manager_v1_create(server->display);
  if (!server->xdg_decoration_manager) {
    wlr_log(WLR_ERROR, "Couldn't create a xdg decoration manager");
    return -1;
  }
  server->xdg_toplevel_decoration.notify = server_new_xdg_toplevel_decoration;
  wl_signal_add(&server->xdg_decoration_manager->events.new_toplevel_decoration, &server->xdg_toplevel_decoration);

  /* cage has this, but I can't find any what 'server_decoration_manager' is or where it is defined */
  server->server_decoration_manager = wlr_server_decoration_manager_create(server->display);
  if (!server->server_decoration_manager) {
    wlr_log(WLR_ERROR, "Could not create a server decoration manager");
    return -1;
  }
  wlr_server_decoration_manager_set_default_mode(server->server_decoration_manager,
  						 WLR_SERVER_DECORATION_MANAGER_MODE_SERVER);

  return 0;
}
