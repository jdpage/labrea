.PHONY: all examples clean

all: labrea

labrea: labrea.scm
	csc -o $@ $^

examples: helloworld rot13

%: examples/%.bf labrea
	./labrea < $< > $<.asm
	yasm -fmacho64 $<.asm -o $<.o
	ld $<.o -o $@

clean:
	rm labrea examples/*.o examples/*.asm helloworld rot13
