all: hello

CFLAGS=-O2 -Ionion/include
LDFLAGS=-lonion -ljson -lmysqlclient -lpthread -L/usr/lib64/mysql/ -Lonion/build/src/onion

hello: hello.c

clean:
	rm -f *.o hello

	
onion:
	git clone https://github.com/davidmoreno/onion.git
	(cd onion && mkdir build && cd build && cmake .. && make -j3)
