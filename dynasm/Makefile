.PHONY: all clean test

CC ?= gcc
CFLAGS = -Wall -Wextra -O2 -fPIC

# Detect OS
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	# macOS
	SHARED_LIB = libdynasm.dylib
	LDFLAGS = -dynamiclib
else
	# Linux and others
	SHARED_LIB = libdynasm.so
	LDFLAGS = -shared
endif

all: $(SHARED_LIB)

$(SHARED_LIB): dynasm.c dynasm.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ dynasm.c

# Racket tests - runs architecture-appropriate tests
test: $(SHARED_LIB)
	raco test .

clean:
	rm -f libdynasm.dylib libdynasm.so *.o
