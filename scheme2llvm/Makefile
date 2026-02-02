.PHONY: runtime test clean

runtime: runtime.dylib runtime.o

runtime.o: runtime.c
	clang -O2 -c runtime.c -o runtime.o

runtime.dylib: runtime.c
	clang -shared -O2 runtime.c -o runtime.dylib

test: runtime.dylib
	raco test tests/

clean:
	rm -rf *.o *.dylib build
