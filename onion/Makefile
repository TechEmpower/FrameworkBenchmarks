all: hello

CFLAGS=-O2 -Ionion/src/
LDFLAGS=-lonion -ljson -lmysqlclient -lpthread -L/usr/lib64/mysql/ -L.

hello.o: onion hello.c 

hello: hello.o libonion_static.a base_html.o fortunes_html.o
	cc hello.o base_html.o fortunes_html.o libonion_static.a -o hello -lpthread -lmysqlclient -ljson -lgnutls -lrt

clean:
	rm -f *.o hello base_html.c fortunes_html.c

	
libonion_static.a: onion
	(cd onion && mkdir -p build && cd build && cmake .. && cd src && make -j4)
	cp onion/build/src/onion/libonion_static.a .

onion:
	git clone https://github.com/davidmoreno/onion.git

otemplate:
	(cd onion/build/tools/otemplate/; make)

base_html.c: otemplate base.html 
	onion/build/tools/otemplate/otemplate base.html base_html.c

fortunes_html.c: otemplate fortunes.html
	onion/build/tools/otemplate/otemplate fortunes.html fortunes_html.c
