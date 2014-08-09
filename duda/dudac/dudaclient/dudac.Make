_PATH     = $(patsubst /%, %, $(CURDIR))
_CC       = @/bin/echo -e "  [\033[33mCC\033[0m]   $@"; $(CC)
_DD       = @/bin/echo -e "  [\033[32mDD\033[0m]   $@"; $(CC)
_CC_QUIET = @/bin/echo -n; $(CC)

all: $(NAME).duda

$(NAME).duda: $(OBJECTS)
	$(_DD) $(CFLAGS) $(DEFS) -shared -o $@ $^ -lc $(LDFLAGS)

.c.o:
	$(_CC) -c $(CFLAGS) $(DEFS) $(INCDIR) -fPIC $<
	$(_CC_QUIET) -MM -MP $(CFLAGS) $(DEFS) $(INCDIR) $*.c -o $*.d > /dev/null &2>&1

clean:
	rm -rf *.o *.d *~ $(NAME).duda
