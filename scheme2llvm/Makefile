.PHONY: test clean

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	SHARED_EXT := dylib
else
	SHARED_EXT := so
endif

runtime.o: runtime.c
	clang -O2 -c runtime.c -o runtime.o

runtime.$(SHARED_EXT): runtime.c
	clang -shared -O2 runtime.c -o runtime.$(SHARED_EXT)

test: runtime.$(SHARED_EXT)
	raco test tests/

clean:
	rm -rf *.o *.dylib *.so build
