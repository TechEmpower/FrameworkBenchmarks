LR       = src/libreactor.o
LRSRV    = src/libreactor-server.o
HELPERS  = src/helpers.o
CFLAGS   = -std=gnu11 -Wall -Wextra -Wpedantic -O3 -g -march=native -flto
LDADD    = -lreactor -ldynamic -lclo

all: libreactor libreactor-server

libreactor: $(LR) $(HELPERS)
	$(CC) -o $@ $^ $(CFLAGS) $(LDADD)

libreactor-server: $(LRSRV) $(HELPERS)
	$(CC) -o $@ $^ $(CFLAGS) $(LDADD)

clean:
	rm -f libreactor libreactor-server $(LR) $(LRSRV) $(HELPERS)
