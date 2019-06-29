CFLAGS := -g -std=c99 -MMD -Wall -Wextra -pedantic $(OPTFLAGS)
LDLIBS := -lncurses

sources = $(wildcard src/*.c)
objects = $(patsubst %.c, %.o, $(sources))

.PHONY: clean

bin/main: $(objects) | bin
	$(LINK.c) $^ -o $@ $(LDLIBS)

clean:
	$(RM) -r bin
	$(RM) src/*.[od]

bin:
	mkdir bin

-include $(patsubst %.c, %.d, $(sources))
