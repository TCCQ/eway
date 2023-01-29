#ifndef EWAY_SERVER
#define EWAY_SERVER
#include <wayland-server-core.h>
#include <stdbool.h>

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

  /* things xdg can request basically. May not be respected */
  struct wl_listener request_move;
  struct wl_listener request_resize;
  struct wl_listener request_maximize;
  struct wl_listener request_fullscreen;
  
  struct wl_listener set_title;
  struct wl_listener set_app_id;

  char title[256]; 		/* fixed length to make things easy */
  char app_id[256];

  int id; 			/* for socket communication */
};

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

  struct wl_list views;
  
  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;
  struct wlr_xdg_decoration_manager_v1 *xdg_decoration_manager;
  struct wl_listener xdg_toplevel_decoration;
  struct wlr_server_decoration_manager *server_decoration_manager;
  struct wl_listener server_decoration;
  
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

};

struct output {
  struct wl_list link; 		/* (?) */
  struct server *server; 	/* what server is this output associated with */
  struct wlr_output *wlr; 	/* (?) */
  struct wlr_scene_output *scene_output; /* what part of the scene does this output cover */

  struct wl_listener frame; 	/* called when the output (monitor) is ready to draw again (refresh rate) */
  struct wl_listener request_state; /* called when mode changes */
  struct wl_listener destroy;	    
};


int server_init (struct server* server);

void server_cleanup (struct server* server);

struct view* validate (int id);

bool allow_manage(int id);

int resize_translate_view(int id, int x, int y, int width, int height);

int close_view(int id);

int hide_view(int id);

int focus_view(int id);
#endif
