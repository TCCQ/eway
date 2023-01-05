#ifndef EWAY_SERVER
#include <wayland-server-core.h>
#define EWAY_SERVER
struct server {
  struct wl_display *display;	/* talk with the comp */
  struct wlr_backend *backend; 	/* how are we rendering pixels */
  struct wlr_renderer *renderer; 	/* what's doing the rendering (?)*/
  struct wlr_allocator *allocator; /* (?) */
  struct wlr_scene *scene;	   /* (?) */
  struct wl_event_loop *wl_event_loop;

  struct wlr_output_layout *output_layout; /* arrangement out outputs */
  struct wl_list outputs;
  struct wl_listener new_output; /* catch a new output being created */
  
  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;
  struct wl_list views;

  struct wlr_seat *seat;
  struct wl_list keyboards;
  struct wlr_cursor *cursor;
  struct wl_listener new_input;
  
  struct wlr_xcursor_manager *cursor_mgr;
  struct wl_listener cursor_motion;
  struct wl_listener cursor_motion_absolute;
  struct wl_listener cursor_button;
  struct wl_listener cursor_axis;
  struct wl_listener cursor_frame;
  
  struct wl_listener request_cursor;
  struct wl_listener request_set_selection;
  

  uint32_t surface_offset; 	/* where to put the next surface  */

  /* struct wl_listener new_surface; /\* someone wants a new surface. in scene_graph but not in tinywl? *\/ */
};


extern struct server *global_server;
#endif
