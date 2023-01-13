WAYLAND_PROTOCOLS=$(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER=$(shell pkg-config --variable=wayland_scanner wayland-scanner)

LIBS=\
	 $(shell pkg-config --cflags --libs wlroots) \
	 $(shell pkg-config --cflags --libs wayland-server) \
	 $(shell pkg-config --cflags --libs xkbcommon)

CC=gcc
CFLAGS=-g -Werror -I. -DWLR_USE_UNSTABLE ${LIBS} -lc
LDFLAGS=${CFLAGS}
LDLIBS=${LIBS}


# example (from tinywl I think):
# $(CC) $(CFLAGS) \
# 	-c -g -Werror -I. \
# 	-DWLR_USE_UNSTABLE \
# 	-o $@ $< \
# 	$(LIBS)

# wayland-scanner is a tool which generates C headers and rigging for Wayland
# protocols, which are specified in XML. wlroots requires you to rig these up
# to your build system yourself and provide them in the include path.

xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

comp.o: comp.c server.h
input.o: input.c input.h server.h
ipc.o: ipc.c server.h ipc.h
server.o: server.c server.h input.h xdg-shell.h ipc.h
xdg-shell.o: xdg-shell.c server.h
comp: comp.o input.o ipc.o server.o xdg-shell.o

clean:
	rm -f comp *.o

.DEFAULT_GOAL=comp

.PHONY: clean
