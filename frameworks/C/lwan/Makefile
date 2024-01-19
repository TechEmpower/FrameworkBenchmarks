.PHONY: all

CFLAGS = -mtune=native -march=native -O3 -fno-plt -flto -ffat-lto-objects -DNDEBUG \
	-falign-functions=32 -malign-data=abi \
	-include /lwan/build/lwan-build-config.h \
	-I /lwan/src/lib \
	$(shell pkg-config mariadb --cflags) \
	$(shell pkg-config sqlite3 --cflags) \
	$(shell pkg-config luajit --cflags)

LDFLAGS = -mtune=native -march=native -O3 -flto -ffat-lto-objects -Wl,-z,now,-z,relro \
	-Wl,-whole-archive /lwan/build/src/lib/liblwan.a -Wl,-no-whole-archive \
	$(shell pkg-config mariadb --libs) \
	$(shell pkg-config sqlite3 --libs) \
	$(shell pkg-config luajit --libs) \
	-lpthread \
	-lz

SRCS = src/techempower.c src/database.c src/json.c

OBJS = $(SRCS:.c=.o)

all: techempower

.c.o: $<
	$(CC) $(CFLAGS) -c $< -o $@

techempower: $(OBJS)
	$(CC) -o techempower $(OBJS) $(LDFLAGS)

clean:
	rm -f techempower $(OBJS)

