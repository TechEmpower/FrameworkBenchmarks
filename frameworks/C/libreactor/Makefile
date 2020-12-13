PROG    = libreactor
OBJS    = src/helpers.o src/main.o
CFLAGS  = -std=gnu11 -Wall -Wextra -Wpedantic -O3 -g -march=native -flto
LDADD   = -lreactor -ldynamic -lclo

$(PROG): $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LDADD)

clean:
	rm -f $(PROG) $(OBJS)
