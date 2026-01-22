.PHONY: all clean test

CC = clang
CFLAGS = -Wall -Wextra -O2 -fPIC

# macOS shared library
DYLIB = libdynasm.dylib

all: $(DYLIB)

$(DYLIB): dynasm.c dynasm.h
	$(CC) $(CFLAGS) -dynamiclib -o $@ dynasm.c

# Racket tests
test: $(DYLIB)
	raco test .

clean:
	rm -f $(DYLIB) *.o
