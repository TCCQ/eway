#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/util/log.h>

#include <xkbcommon/xkbcommon.h>

struct server {
  struct wl_display *display;	/* talk with the comp */
  struct wlr_backend *backend; 	/* how are we rendering pixels */
  struct wlr_renderer *renderer; 	/* what's doing the rendering (?)*/
  struct wlr_allocator *allocator; /* (?) */
  struct wlr_scene *scene;	   /* (?) */

  struct wlr_output_layout *output_layout; /* arrangement out outputs */
  struct wl_list outputs;
  
  
  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;
  struct wl_list views;

  struct wlr_seat *seat;
  struct wl_listener new_input;
  struct wl_list keyboards;
  /*
   * struct wl_listener request_cursor;
   * struct wl_listener request_set_selection;
   */


  
  uint32_t surface_offset; 	/* (?) */

  struct wl_listener new_output; /* catch a new output being created */
  struct wl_listener new_surface; /* someone wants a new surface */
};

struct surface {
  struct wlr_surface *wlr; 	/* (?) */
  struct wlr_scene_surface *scene_surface;	/* where it is in the scene (?) */
  /*
   * we don't want a border on our surfaces
   */
  struct wlr_scene_rect *border; 	/* what box it takes up */
   
  struct wl_list link;			/* (?) */

  struct wl_listener commit;	/* this surface is ready to be shown / rendered */
  struct wl_listener destroy; 	/* this surface is ready to be destroyed */
};

struct output {
  struct wl_list link; 		/* (?) */
  struct server *server; 	/* what server is this output associated with */
  struct wlr_output *wlr; 	/* (?) */
  struct wlr_scene_output *scene_output; /* (?) */

  struct wl_listener frame; 	/* called when the output (monitor) is ready to draw again (refresh rate) */
};

struct view {
  /* I guess this is a higher level wrapper for a surface? between a
     surface and a shell or something else */
  struct wl_list link;
  struct server *server;
  struct wlr_xdg_toplevel *xdg_toplevel;
  struct wlr_scene_tree *scene_tree;
  struct wl_listener map; 	/* show this view */
  struct wl_listener unmap; 	/* hide this view */
  struct wl_listener destroy; 	/* we are done */
  int x, y;
};

struct keyboard {
  struct wl_list link;
  struct server *server;
  struct wlr_keyboard *wlr_keyboard;

  struct wl_listener modifiers;
  struct wl_listener key;
  struct wl_listener destroy;
};

/*
 * new outputs, and other server handle_ type stuff
 */
/* {{{ */

static void output_handle_frame (struct wl_listener *listener, void *data) {
  struct output *output = wl_container_of(listener, output, frame);

  if (!wlr_scene_output_commit(output->scene_output)) {
    /* we aren't ready to draw anything */
    return; 
  }

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  wlr_scene_output_send_frame_done(output->scene_output, &now);
}

static void server_handle_new_output (struct wl_listener *listener, void *data) {
  struct server *server = wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

  wlr_output_init_render(wlr_output, server->allocator, server->renderer);

  struct output *output = calloc(1, sizeof(struct output));

  output->wlr = wlr_output;
  output->server = server;
  output->frame.notify = output_handle_frame;
  wl_signal_add(&wlr_output->events.frame, &output->frame);

  output->scene_output = wlr_scene_output_create(server->scene, wlr_output);

  /* set output modes (monitor resolution / refresh rate) */
  if (!wl_list_empty(&wlr_output->modes)) {
    struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
    wlr_output_set_mode(wlr_output, mode);
    wlr_output_enable(wlr_output, true);
    if (!wlr_output_commit(wlr_output)) {
      return;
    }
  }

  wlr_output_create_global(wlr_output);
  wl_list_insert(&server->outputs, &output->link);
  wlr_output_layout_add_auto(server->output_layout, wlr_output);
}

static void surface_handle_commit (struct wl_listener *listener, void *data) {
  struct surface *surface = wl_container_of(listener, surface, commit);
  /* no border */
  wlr_scene_rect_set_size(surface->border, surface->wlr->current.width, surface->wlr->current.height);
}

static void surface_handle_destroy (struct wl_listener *listener, void *data) {
  struct surface *surface = wl_container_of(listener, surface, destroy);
  wlr_scene_node_destroy(&surface->scene_surface->buffer->node);
  wlr_scene_node_destroy(&surface->border->node);
  wl_list_remove(&surface->destroy.link);
  wl_list_remove(&surface->link);
  free(surface);
}

static void server_handle_new_surface(struct wl_listener *listener, void *data) {
  struct server *server = wl_container_of(listener, server, new_surface);
  struct wlr_surface *wlr_surface = data;

  int pos = server->surface_offset;
  server->surface_offset += 50;

  struct surface *surface = calloc(1, sizeof(struct surface));
  surface->wlr = wlr_surface;
  surface->commit.notify = surface_handle_commit;
  wl_signal_add(&wlr_surface->events.commit, &surface->commit);
  surface->destroy.notify = surface_handle_destroy;
  wl_signal_add(&wlr_surface->events.destroy, &surface->destroy);

  /* would handle border creation here if we had borders */
  surface->border = wlr_scene_rect_create(&server->scene->tree, 0, 0, (float[4]){0.5f, 0.5f, 0.5f, 1});
  wlr_scene_node_set_position(&surface->border->node, pos, pos);

  surface->scene_surface = wlr_scene_surface_create(&server->scene->tree, wlr_surface);

  wlr_scene_node_set_position(&surface->scene_surface->buffer->node, pos, pos);
}

/* }}} */

/*
 * xdg_shell stuff. mostly stolen from tinywl
 */
/* {{{ */

static void xdg_toplevel_map (struct wl_listener *listener, void *data) {
  /* surface is mapped (ready to display) */
  struct view *view = wl_container_of(listener, view, map);

  wl_list_insert(&view->server->views, &view->link);

  /* focus? */
}

static void xdg_toplevel_unmap (struct wl_listener *listener, void *data) {
  struct view *view = wl_container_of(listener, view, unmap);

  wl_list_remove(&view->link);
}

static void xdg_toplevel_destroy (struct wl_listener *listener, void *data) {
  struct view *view = wl_container_of(listener, view, destroy);

  wl_list_remove(&view->map.link);
  wl_list_remove(&view->unmap.link);
  wl_list_remove(&view->destroy.link);
  
  free(view);
}

static void server_new_xdg_surface (struct wl_listener *listener, void *data) {
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
}

/* }}} */

/*
 * input stuff. stolen from tinywl
 */

static void keyboard_focus_to_view (struct view *view, struct wlr_surface *surface) {
  if (!view) {
    return;
  }
  struct server *server = view->server;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;
  if (prev_surface == surface) {
    /* dont' refocus */
    return;
  }

  if (prev_surface) {
    /* remove focus from prev */
    struct wlr_xdg_surface *previous = wlr_xdg_surface_from_wlr_surface(seat->keyboard_state.focused_surface);
    assert(previous->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);
    wlr_xdg_toplevel_set_activated(previous->toplevel, false);
  }
  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  /* raise view */
  wlr_scene_node_raise_to_top(&view->scene_tree->node);
  wl_list_remove(&view->link);
  wl_list_insert(&server->views, &view->link);
  /* not really sure what is happening here ^ */
  wlr_xdg_toplevel_set_activated(view->xdg_toplevel, true);
  /* wlroots will automatically forward key inputs elsewhere */
  if (keyboard) {
    wlr_seat_keyboard_notify_enter(seat, view->xdg_toplevel->base->surface,
				   keyboard->keycodes, keyboard->num_keycodes, &keyboard->modifiers);
  }
}

static void keyboard_handle_modifiers (struct wl_listener *listener, void *data) {
  /* raised when any modifier is pressed */
  struct keyboard *keyboard = wl_container_of(listener, keyboard, modifiers);
  /* workaround for seats only allowing one keyboard */
  wlr_seat_set_keyboard(keyboard->server->seat, keyboard->wlr_keyboard);
  /* send to client */
  wlr_seat_keyboard_notify_modifiers(keyboard->server->seat, &keyboard->wlr_keyboard->modifiers);
}

static bool handle_keybinding (struct server *server, uint32_t modifiers, xkb_keysym_t sym) {
  /* should the compositor consume this keystroke? if so, do the action */
  if (modifiers & WLR_MODIFIER_ALT) {
    switch (sym) {
    case XKB_KEY_Escape:
      wl_display_terminate(server->display);
      return true;
    case XKB_KEY_F1:
      /* Cycle to the next view */
      if (!wl_list_length(&server->views) < 2) {
	struct view *next_view = wl_container_of(server->views.prev, next_view, link);
	keyboard_focus_to_view(next_view, next_view->xdg_toplevel->base->surface);
      }
      return true;
    default:
      return false;
    }
  }
  return false;
}

static void keyboard_handle_key (struct wl_listener *listener, void *data) {
  /* raised when key is pressed or released */
  struct keyboard *keyboard = wl_container_of(listener, keyboard, key);
  struct server *server = keyboard->server;
  struct wlr_keyboard_key_event *event = data;
  struct wlr_seat *seat = server->seat;

  /* libinput -> xkbcommon */
  uint32_t keycode = event->keycode + 8;
  /* get keysyms based on keymap */
  const xkb_keysym_t *syms;
  int nsyms = xkb_state_key_get_syms(keyboard->wlr_keyboard->xkb_state, keycode, &syms);
  uint32_t modifiers = wlr_keyboard_get_modifiers(keyboard->wlr_keyboard);
  bool handled = false;
  
  if (event->state == WL_KEYBOARD_KEY_STATE_PRESSED) {
    /* only trigger on downstroke */
    for (int i = 0; i < nsyms; i++) {
      handled = handle_keybinding(server, modifiers, syms[i]);
    }
  }
  /* I don't think I understand how `handled` works here, so for now this follows what tinywl does (mostly) */
  if (!handled) {
    /* pass to client */
    wlr_seat_set_keyboard(seat, keyboard->wlr_keyboard);
    wlr_seat_keyboard_notify_key(seat, event->time_msec, event->keycode, event->state);
  }
}

static void keyboard_handle_destory (struct wl_listener *listener, void *data) {
  struct keyboard *keyboard = wl_container_of(listener, keyboard, destroy);
  wl_list_remove(&keyboard->modifiers.link);
  wl_list_remove(&keyboard->key.link);
  wl_list_remove(&keyboard->destroy.link);
  wl_list_remove(&keyboard->link);
  free(keyboard);
}

static void server_new_keyboard (struct server *server, struct wlr_input_device *device) {
  struct wlr_keyboard *wlr_keyboard = wlr_keyboard_from_input_device(device);
  struct keyboard *keyboard = calloc(1, sizeof(struct keyboard));
  keyboard->server = server;
  keyboard->wlr_keyboard = wlr_keyboard;

  /* pick and assign keymap. assumes default */
  struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  struct xkb_keymap *keymap = xkb_keymap_new_from_names(context, NULL, XKB_KEYMAP_COMPILE_NO_FLAGS);

  wlr_keyboard_set_keymap(wlr_keyboard, keymap);
  xkb_keymap_unref(keymap);
  xkb_context_unref(context);
  wlr_keyboard_set_repeat_info(wlr_keyboard, 25, 600);
  /* parameterize ^ */

  /* set listeners */
  keyboard->modifiers.notify = keyboard_handle_modifiers;
  wl_signal_add(&wlr_keyboard->events.modifiers, &keyboard->modifiers);
  keyboard->key.notify = keyboard_handle_key;
  wl_signal_add(&wlr_keyboard->events.key, &keyboard->key);
  keyboard->destroy.notify = keyboard_handle_destory;
  wl_signal_add(&device->events.destroy, &keyboard->destroy);

  wlr_seat_set_keyboard(server->seat, keyboard->wlr_keyboard);

  wl_list_insert(&server->keyboards, &keyboard->link);
}

static void server_new_input (struct wl_listener *listener, void *data) {
  /* raised on new input device */
  struct server *server = wl_container_of(listener, server, new_input);
  struct wlr_input_device *device = data;
  switch (device->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    server_new_keyboard(server, device);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    /* later */
    break;
  default:
    break;
  }
  /* let the wlr_seat our capabilities, which is communiciated
   * to the client. we will always have a cursor, even if there
   * are no pointer devices, so start with that. */
  uint32_t caps = WL_SEAT_CAPABILITY_POINTER;
  if (!wl_list_empty(&server->keyboards)) {
    caps |= WL_SEAT_CAPABILITY_KEYBOARD;
  }
  wlr_seat_set_capabilities(server->seat, caps);
}

int main(int argc, char** argv) {
  wlr_log_init(WLR_DEBUG, NULL);

  char *startup_cmd = NULL;

  int c;
  while ((c = getopt(argc, argv, "s:")) != -1) {
    switch (c) {
    case 's':
      startup_cmd = optarg;
      break;
    default:
      printf("usage: %s -s startup command\n", argv[0]);
      return EXIT_FAILURE;
    }
  }
  if (optind < argc) {
    printf("usage: %s -s startup command\n", argv[0]);
    return EXIT_FAILURE;
  }

  struct server server = {0};
  server.surface_offset = 0;
  server.display = wl_display_create();
  server.backend = wlr_backend_autocreate(server.display, NULL);
  server.renderer = wlr_renderer_autocreate(server.backend);
  wlr_renderer_init_wl_display(server.renderer, server.display);
  server.allocator = wlr_allocator_autocreate(server.backend, server.renderer);

  struct wlr_compositor *compositor = wlr_compositor_create(server.display, server.renderer);
  struct wlr_subcompositor *subcompositor = wlr_subcompositor_create(server.display);

  server.output_layout = wlr_output_layout_create();
  wl_list_init(&server.outputs);
  server.new_output.notify = server_handle_new_output;
  wl_signal_add(&server.backend->events.new_output, &server.new_output);
  
  server.scene = wlr_scene_create();
  wlr_scene_attach_output_layout(server.scene, server.output_layout);

  wl_list_init(&server.views);
  server.xdg_shell = wlr_xdg_shell_create(server.display, 3); 
  server.new_xdg_surface.notify = server_new_xdg_surface;
  wl_signal_add(&server.xdg_shell->events.new_surface, &server.new_xdg_surface);  

  /* add cursor stuff here */
  
  /* configure a single seat, which contains all input devices */  
  wl_list_init(&server.keyboards);
  server.new_input.notify = server_new_input;
  wl_signal_add(&server.backend->events.new_input, &server.new_input);
  server.seat = wlr_seat_create(server.display, "seat0");
  /*
   * server.request_cursor.notify = seat_request_cursor;
   * wl_signal_add(&server.seat->events.request_set_cursor,
   * 		&server.request_cursor);
   * server.request_set_selection.notify = seat_request_set_selection;
   * wl_signal_add(&server.seat->events.request_set_selection,
   * 		&server.request_set_selection);
   */

  
  const char *socket = wl_display_add_socket_auto(server.display);
  if (!socket) {
    wl_display_destroy(server.display);
    return EXIT_FAILURE;
  }

  if (!wlr_backend_start(server.backend)) {
    wl_display_destroy(server.display);
    return EXIT_FAILURE;
  }
  
  setenv("WAYLAND_DISPLAY", socket, true);
  if (startup_cmd != NULL) {
    if (fork() == 0) {
      execl("/bin/sh", "/bin/sh", "-c", startup_cmd, (void*) NULL);
    }
  }

  wlr_log(WLR_INFO, "Running Wayland compositor on WAYLAND_DISPLAY=%s", socket);
  wl_display_run(server.display);

  wl_display_destroy_clients(server.display);
  wl_display_destroy(server.display);
  return EXIT_SUCCESS;
}
