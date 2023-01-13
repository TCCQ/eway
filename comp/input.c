#define _POSIX_C_SOURCE 200112L
#include <assert.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_data_device.h>

#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/backend.h>
#include <wlr/util/log.h>


#include "input.h"
#include "server.h"
/*
 * input stuff. stolen from tinywl
 */

void keyboard_focus_to_view (struct view *view, struct wlr_surface *surface) {
  if (view == NULL) {
    return;
  }
  struct server *server = view->server;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;
  if (prev_surface == surface) {
    /* don't refocus */
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

void keyboard_handle_modifiers (struct wl_listener *listener, void *data) {
  /* raised when any modifier is pressed */
  struct keyboard *keyboard = wl_container_of(listener, keyboard, modifiers);
  /* workaround for seats only allowing one keyboard */
  wlr_seat_set_keyboard(keyboard->server->seat, keyboard->wlr_keyboard);
  /* send to client */
  wlr_seat_keyboard_notify_modifiers(keyboard->server->seat, &keyboard->wlr_keyboard->modifiers);
}

bool handle_keybinding (struct server *server, uint32_t modifiers, xkb_keysym_t sym) {
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

void keyboard_handle_key (struct wl_listener *listener, void *data) {
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
      wlr_log(WLR_DEBUG, "key down: %d, focus: %d", syms[i], server->seat->keyboard_state.focused_surface);
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

void keyboard_handle_destory (struct wl_listener *listener, void *data) {
  struct keyboard *keyboard = wl_container_of(listener, keyboard, destroy);
  wl_list_remove(&keyboard->modifiers.link);
  wl_list_remove(&keyboard->key.link);
  wl_list_remove(&keyboard->destroy.link);
  wl_list_remove(&keyboard->link);
  free(keyboard);
}

void server_new_keyboard (struct server *server, struct wlr_input_device *device) {
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

void server_new_pointer (struct server *server, struct wlr_input_device *device) {
  wlr_cursor_attach_input_device(server->cursor, device);
}

void server_new_input (struct wl_listener *listener, void *data) {
  /* raised on new input device */
  struct server *server = wl_container_of(listener, server, new_input);
  struct wlr_input_device *device = data;
  switch (device->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    server_new_keyboard(server, device);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    server_new_pointer(server, device);
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

void seat_request_cursor (struct wl_listener *listener, void *data) {
  struct server *server = wl_container_of(listener, server, request_cursor);
  /* raised when client provides a cursor image */
  struct wlr_seat_pointer_request_set_cursor_event *event = data;
  struct wlr_seat_client *focused_client = server->seat->pointer_state.focused_client;
  /* only allowed by the current pointer focused client */
  if (focused_client == event->seat_client) {
    wlr_cursor_set_surface(server->cursor, event->surface, event->hotspot_x, event->hotspot_y);
  }
}

void seat_request_set_selection (struct wl_listener *listener, void *data) {
  /* raised by seat when client tries to set setselection (copy something) */
  struct server *server = wl_container_of(listener, server, request_set_selection);
  struct wlr_seat_request_set_selection_event *event = data;
  wlr_seat_set_selection(server->seat, event->source, event->serial);
}

struct view *desktop_view_at (struct server *server, double lx, double ly,
			      struct wlr_surface **surface, double *sx, double *sy) {
  /* topmost node in the scene at layout coords (ony surface nodes) */
  struct wlr_scene_node *node = wlr_scene_node_at(&server->scene->tree.node, lx, ly, sx, sy);
  if (node == NULL || node->type != WLR_SCENE_NODE_BUFFER) {
    return NULL;
  }
  struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
  struct wlr_scene_surface *scene_surface = wlr_scene_surface_from_buffer(scene_buffer);
  if (!scene_surface) {
    return NULL;
  }

  *surface = scene_surface->surface;
  /* node for view at this surface tree. */
  struct wlr_scene_tree *tree = node->parent;
  while (tree != NULL && tree->node.data == NULL) {
    tree = tree->node.parent;
  }
  return tree->node.data;
}

void process_cursor_motion (struct server *server, uint32_t time) {
  /* find view under and pass */
  double sx, sy;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *surface = NULL;
  struct view *view = desktop_view_at(server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  if (!view) {
    /* no view under, set cursor to default */
    wlr_xcursor_manager_set_cursor_image(server->cursor_mgr, "left_ptr", server->cursor);
  }
  if (surface) {
    /* send pointer / motion events */
    /* enter to give pointer focus */
    wlr_seat_pointer_notify_enter(seat, surface, sx, sy);
    wlr_seat_pointer_notify_motion(seat, time, sx, sy);
  } else {
    /* clear focus so that events are not sent to last client */
    wlr_seat_pointer_clear_focus(seat);
  }
}

void server_cursor_motion (struct wl_listener *listener, void *data) {
  /* raised by relative cursor motion */
  struct server *server =  wl_container_of(listener, server, cursor_motion);
  struct wlr_pointer_motion_event *event = data;
  /* constraining to output and whatnot is automatic */
  wlr_cursor_move(server->cursor, &event->pointer->base, event->delta_x, event->delta_y);
  process_cursor_motion(server, event->time_msec);
}

void server_cursor_motion_absolute (struct wl_listener *listener, void *data) {
  /* raised by absolute event (eg when run inside a window or with special hardware) */
  struct server *server =  wl_container_of(listener, server, cursor_motion_absolute);
  struct wlr_pointer_motion_absolute_event *event = data;
  wlr_cursor_warp_absolute(server->cursor, &event->pointer->base, event->x, event->y);
  process_cursor_motion(server, event->time_msec);
}

void server_cursor_button (struct wl_listener *listener, void *data) {
  struct server *server = wl_container_of(listener, server, cursor_button);
  struct wlr_pointer_button_event *event = data;
  /* notify client with focus */
  wlr_seat_pointer_notify_button(server->seat, event->time_msec, event->button, event->state);
  double sx, sy;
  struct wlr_surface *surface = NULL;
  struct view *view = desktop_view_at(server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  if (event->state = WLR_BUTTON_PRESSED) {
    /* focus (keyboard) if pressed */
    keyboard_focus_to_view(view, surface);
  }
}

void server_cursor_axis (struct wl_listener *listener, void *data) {
  /* axis event (scroll wheel) */
  struct server *server = wl_container_of(listener, server, cursor_axis);
  struct wlr_pointer_axis_event *event = data;
  /* pass to client */
  wlr_seat_pointer_notify_axis(server->seat, event->time_msec, event->orientation,
			       event->delta, event->delta_discrete, event->source);
}

void server_cursor_frame (struct wl_listener *listener, void *data) {
  /* raised when pointer emmits frame (group inputs) */
  struct server *server = wl_container_of(listener, server, cursor_frame);
  /* notify */
  wlr_seat_pointer_notify_frame(server->seat);
}

void server_init_input (struct server* server) {
    /* cursor stuff */
  server->cursor = wlr_cursor_create();
  wlr_cursor_attach_output_layout(server->cursor, server->output_layout);
  server->cursor_mgr = wlr_xcursor_manager_create(NULL, 24);
  wlr_xcursor_manager_load(server->cursor_mgr, 1);

  server->cursor_motion.notify = server_cursor_motion;
  wl_signal_add(&server->cursor->events.motion, &server->cursor_motion);
  server->cursor_motion_absolute.notify = server_cursor_motion_absolute;
  wl_signal_add(&server->cursor->events.motion_absolute,
		&server->cursor_motion_absolute);
  server->cursor_button.notify = server_cursor_button;
  wl_signal_add(&server->cursor->events.button, &server->cursor_button);
  server->cursor_axis.notify = server_cursor_axis;
  wl_signal_add(&server->cursor->events.axis, &server->cursor_axis);
  server->cursor_frame.notify = server_cursor_frame;
  wl_signal_add(&server->cursor->events.frame, &server->cursor_frame);
  
  /* configure a single seat, which contains all input devices */  
  wl_list_init(&server->keyboards);
  server->new_input.notify = server_new_input;
  wl_signal_add(&server->backend->events.new_input, &server->new_input);
  server->seat = wlr_seat_create(server->display, "seat0");
  server->request_cursor.notify = seat_request_cursor;
  wl_signal_add(&server->seat->events.request_set_cursor,
  		&server->request_cursor);
  server->request_set_selection.notify = seat_request_set_selection;
  wl_signal_add(&server->seat->events.request_set_selection,
  		&server->request_set_selection);

}
