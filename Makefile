.PHONY: all examples clean

all: helloworld rot13

%: examples/%.bf labrea.scm
	./labrea -fnasm -o $<.asm $<
	yasm -fmacho64 $<.asm -o $<.o
	ld $<.o -o $@

clean:
	-rm examples/*.o examples/*.asm helloworld rot13
