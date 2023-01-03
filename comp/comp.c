#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
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
#include <wlr/util/log.h>

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

  /* input stuff goes here */
  
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

/* now we need xdg stuff. this will be mostly stolen from  tinywl */

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
  server.xdg_shell = wlr_xdg_shell_create(server.display, 3); /* ? */
  server.new_xdg_surface.notify = server_new_xdg_surface;
  wl_signal_add(&server.xdg_shell->events.new_surface, &server.new_xdg_surface);  
  
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
