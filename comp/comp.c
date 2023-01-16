#define _POSIX_C_SOURCE 200112L
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
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/util/log.h>

#include <xkbcommon/xkbcommon.h>

#include "server.h"

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

  struct server server;
  if (server_init(&server)) {
    wlr_log(WLR_ERROR, "init error, stopping");
    return EXIT_FAILURE;
  }

  if (startup_cmd == NULL) {
    startup_cmd = "emacs --eval=\"(when (load-file \\\"~/Desktop/eway/eway.el\\\") (eway-init-socket))\"";
  }
  if (startup_cmd != NULL) {
    if (fork() == 0) {
      execl("/bin/sh", "/bin/sh", "-c", startup_cmd, (void*) NULL);
    }
  }

  wl_display_run(server.display);

  server_cleanup(&server);
  
  return EXIT_SUCCESS;
}
