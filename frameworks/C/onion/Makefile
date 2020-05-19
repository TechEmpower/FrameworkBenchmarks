.PHONY: all
all: hello

CFLAGS=-O2 -Ionion/src/

hello: hello.o libonion_static.a base_html.o fortunes_html.o
	cc hello.o base_html.o fortunes_html.o libonion_static.a -o hello -lpthread `mysql_config --libs` `pkg-config json --libs` -lgnutls -lgcrypt -lrt

.PHONY: clean
clean:
	rm -fr *.o hello base_html.c fortunes_html.c

base_html.c: base.html
	onion/build/tools/otemplate/otemplate base.html base_html.c

fortunes_html.c: fortunes.html
	onion/build/tools/otemplate/otemplate fortunes.html fortunes_html.c
