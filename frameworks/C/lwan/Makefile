.PHONY: all

CFLAGS = -mtune=native -march=native -O3 -flto -ffat-lto-objects -DNDEBUG \
	-include /lwan/build/lwan-build-config.h \
	-I /lwan/src/lib \
	`pkg-config mariadb --cflags` \
	`pkg-config sqlite3 --cflags`

LDFLAGS = -mtune=native -march=native -O3 -flto -ffat-lto-objects \
	-Wl,-whole-archive /lwan/build/src/lib/liblwan.a -Wl,-no-whole-archive \
	`pkg-config mariadb --libs` \
	`pkg-config sqlite3 --libs` \
	-lpthread \
	-lz

SRCS = src/techempower.c src/database.c src/json.c

OBJS = $(SRCS:.c=.o)

all: techempower

.c.o: $<
	$(CC) $(CFLAGS) -c $< -o $@

techempower: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o techempower

clean:
	rm -f techempower $(OBJS)

