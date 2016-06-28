all: hello

CFLAGS=-O2 -Ionion/src/
LDFLAGS=-lonion -ljson -lmysqlclient -lpthread -L/usr/lib64/mysql/ -L.

hello.o: onion hello.c 

hello: hello.o libonion_static.a base_html.o fortunes_html.o
	cc hello.o base_html.o fortunes_html.o libonion_static.a -o hello -lpthread `mysql_config --libs` `pkg-config json --libs` -lgnutls -lgcrypt -lrt

clean:
	rm -fr *.o hello base_html.c fortunes_html.c libonion_static.a onion


libonion_static.a: onion
	(cd onion && mkdir -p build && cd build && cmake .. -DCMAKE_C_FLAGS="-Werror" -DCMAKE_CXX_FLAGS="-Werror" && make VERBOSE=1)
	cp onion/build/src/onion/libonion_static.a .

onion:
	git clone --depth 1 https://github.com/davidmoreno/onion.git

otemplate:
	(cd onion/build/tools/otemplate/)

base_html.c: otemplate base.html 
	onion/build/tools/otemplate/otemplate base.html base_html.c

fortunes_html.c: otemplate fortunes.html
	onion/build/tools/otemplate/otemplate fortunes.html fortunes_html.c
