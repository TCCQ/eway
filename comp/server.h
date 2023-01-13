#ifndef EWAY_SERVER
#define EWAY_SERVER
#include <wayland-server-core.h>
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

  struct wl_listener new_surface; /* someone wants a new surface. in scene_graph but not in tinywl? */
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

int server_init (struct server* server);

void server_cleanup (struct server* server);

/* emacs ipc stuff, called from ipc.c but needs to see the internal state of the server */
typedef void* eway_id_t;

int resize_translate_view(eway_id_t id, int x, int y, int width, int height);

int close_view(eway_id_t id);

int hide_view(eway_id_t id);

int focus_view(eway_id_t id);
#endif
