all: hello

CFLAGS=-O2 -Ionion/src/
LDFLAGS=-lonion -ljson -lmysqlclient -lpthread -L/usr/lib64/mysql/ -L.

hello.o: onion hello.c 

hello: hello.o libonion_static.a
	cc hello.o libonion_static.a -o hello -lpthread -lmysqlclient -ljson -lgnutls -lrt

clean:
	rm -f *.o hello

	
libonion_static.a: onion
	(cd onion && mkdir -p build && cd build && cmake .. && cd src && make -j4)
	cp onion/build/src/onion/libonion_static.a .

onion:
	git clone https://github.com/davidmoreno/onion.git

