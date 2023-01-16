#define _POSIX_C_SOURCE 200112L

#include <time.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>

#include "server.h"
#include "input.h"
#include "xdg-shell.h"
#include "ipc.h"
#include "id.h"
/*
 * new outputs, and other server handle_ type stuff
 */

void output_handle_frame (struct wl_listener *listener, void *data) {
  struct output *output = wl_container_of(listener, output, frame);

  struct wlr_scene *scene = output->server->scene;
  struct wlr_scene_output *scene_output = wlr_scene_get_scene_output(scene, output->wlr);
  wlr_scene_output_commit(scene_output);
  /* if (!wlr_scene_output_commit(output->scene_output)) { */
    /* we aren't ready to draw anything */
    /* return; */
  /* } */

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  wlr_scene_output_send_frame_done(scene_output, &now);
}

void output_request_state (struct wl_listener *listener, void *data) {
  /* This function is called when the backend requests a new state for
   * the output. For example, Wayland and X11 backends request a new mode
   * when the output window is resized. */
  struct output *output = wl_container_of(listener, output, request_state);
  const struct wlr_output_event_request_state *event = data;
  wlr_output_commit_state(output->wlr, event->state);
}

static void output_destroy(struct wl_listener *listener, void *data) {
	struct output *output = wl_container_of(listener, output, destroy);

	wl_list_remove(&output->frame.link);
	wl_list_remove(&output->request_state.link);
	wl_list_remove(&output->destroy.link);
	wl_list_remove(&output->link);
	free(output);
}

void server_handle_new_output (struct wl_listener *listener, void *data) {
  struct server *server = wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

  wlr_output_init_render(wlr_output, server->allocator, server->renderer);

  /* handle not having mode */
  if (!wl_list_empty(&wlr_output->modes)) {
    struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
    wlr_output_set_mode(wlr_output, mode);
    wlr_output_enable(wlr_output, true);
    if (!wlr_output_commit(wlr_output)) {
      return;
    }
  }
  
  struct output *output = calloc(1, sizeof(struct output));

  output->wlr = wlr_output;
  output->server = server;
  output->frame.notify = output_handle_frame;
  wl_signal_add(&wlr_output->events.frame, &output->frame);
  output->request_state.notify = output_request_state;
  wl_signal_add(&wlr_output->events.request_state, &output->request_state);
  output->destroy.notify = output_destroy;
  wl_signal_add(&wlr_output->events.destroy, &output->destroy);
  
  wl_list_insert(&server->outputs, &output->link);
  
  output->scene_output = wlr_scene_output_create(server->scene, wlr_output);

  /* unsure if I need this */
  /*
   * wlr_output_create_global(wlr_output);
   */
  
  wlr_output_layout_add_auto(server->output_layout, wlr_output);
}

/* interal use stuff */
struct view* validate (int id) {
  struct view* passed = id_get(id);
  if (!passed) {
    return NULL;
  }
  return passed;
  /*
   * struct view* match;
   * wl_list_for_each(match, server->views, link) {
   *   if (match == passed) {
   *     return match;
   *   }
   * }
   * return NULL;
   */
}

/* publicly visible stuff: Init and requests */

/* server internals for responding to requests via the socket */

int resize_translate_view(int id, int x, int y, int width, int height) {
  wlr_log(WLR_DEBUG, "resize requested %d %d %d %d %d", id, x, y, width, height);

  struct view *view = validate(id);
  if (!view) {
    return -1;
  }

  wlr_scene_node_set_position(&view->scene_tree->node, x, y);
  wlr_xdg_toplevel_set_size(view->xdg_toplevel, width, height);
  
  return 0;
}

int close_view(int id) {
  wlr_log(WLR_DEBUG, "close req %d", id);
  return 0;
}

int hide_view(int id) {
  wlr_log(WLR_DEBUG, "hide req %d", id);
  return 0;
}

int focus_view(int id) {
  wlr_log(WLR_DEBUG, "focus req %d", id);
  return 0;
}

int server_init (struct server* server) {
  server->display = wl_display_create();
  server->wl_event_loop = wl_display_get_event_loop(server->display);
  server->backend = wlr_backend_autocreate(server->display, NULL);
  if (!server->backend) {
    wlr_log(WLR_ERROR, "Could not create wlr_backend");
    return -1;
  }
  server->renderer = wlr_renderer_autocreate(server->backend);
  if (!server->renderer) {
    wlr_log(WLR_ERROR, "Could not create wlr_renderer");
    return -1;
  }
  wlr_renderer_init_wl_display(server->renderer, server->display);
  server->allocator = wlr_allocator_autocreate(server->backend, server->renderer);
  if (!server->allocator) {
    wlr_log(WLR_ERROR, "Could not create wlr_allocator");
    return -1;
  }

  wlr_compositor_create(server->display, server->renderer);
  wlr_subcompositor_create(server->display);
  wlr_data_device_manager_create(server->display);

  server->output_layout = wlr_output_layout_create();
  wl_list_init(&server->outputs);
  server->new_output.notify = server_handle_new_output;
  wl_signal_add(&server->backend->events.new_output, &server->new_output);
  
  server->scene = wlr_scene_create();
  wlr_scene_attach_output_layout(server->scene, server->output_layout);
  wl_list_init(&server->views);

  /* core initialized */
  if (server_init_xdg_shell(server)) {
    wlr_log(WLR_ERROR, "Could not setup xdg_shell");
    return -1;
  }
  server_init_input(server);

  /* ipc */
  init_socket(server);
  /* id */
  if (id_init()) {
    wlr_log(WLR_ERROR, "id init error");
    return -1;
  }

  /* final setup */
  const char *socket = wl_display_add_socket_auto(server->display);
  if (!socket) {
    wlr_backend_destroy(server->backend);
    return -1;
  }

  if (!wlr_backend_start(server->backend)) {
    wlr_backend_destroy(server->backend);
    wl_display_destroy(server->display);
    return -1;
  }

  setenv("WAYLAND_DISPLAY", socket, true);
  wlr_log(WLR_INFO, "Running Wayland compositor on WAYLAND_DISPLAY=%s", socket);
  
  return 0;
}

void server_cleanup (struct server* server) {
  wl_display_destroy_clients(server->display);
  wl_display_destroy(server->display);
}
