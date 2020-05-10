PROG    = libreactor
OBJS    = src/setup.o src/main.o
CFLAGS  = -std=gnu11 -Wall -Wextra -Wpedantic -O3
LDFLAGS = -pthread
LDADD   = -lreactor -ldynamic -lclo -flto

$(PROG): $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LDADD)

clean:
	rm -f $(PROG) $(OBJS)
