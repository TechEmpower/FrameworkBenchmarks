CC     = gcc-6
PROG   = libreactor
OBJS   = src/setup.o src/main.o
CFLAGS = -std=gnu11 -Wall -O3 -march=native -mtune=native -flto -fuse-linker-plugin -Isrc
LDADD  = -lreactor -ldynamic -lclo

$(PROG): $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS) $(LDADD)

clean:
	rm -f $(PROG) $(OBJS)
