CC      = gcc -g -Wall -DDEBUG
INCDIR  = ../src -I../../../src/include/ -I../
SOURCES = hello.c
OBJECTS = hello.o

all: hello.duda

hello.duda: $(OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) $(DEFS) -shared -Wl -o $@ $^ -lc

.c.o: $(SOURCES)
	$(CC) $(CFLAGS) $(LDFLAGS) $(DEFS) -I$(INCDIR) -fPIC -c $<

clean:
	rm -rf *.o *~ *.*duda*
