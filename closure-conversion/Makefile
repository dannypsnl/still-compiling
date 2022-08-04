test.ll::
	racket llvm.rkt > test.ll
test.s: test.ll
	llc test.ll
runtime.o:
	clang -O3 -c runtime.c
test.o: test.s
	clang -O3 -c test.s

a.out: test.o runtime.o
	clang *.o

run: a.out
	./a.out

clean:
	rm *.o
	rm *.s
	rm *.ll
	rm *.out

