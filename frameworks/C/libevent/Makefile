all: server.c
	gcc -O3 -o server server.c -levent

clean:
	rm -f server

run:
	make all
	./server
