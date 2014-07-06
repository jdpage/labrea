.PHONY: all examples clean

all: helloworld rot13

%: examples/%.o
	ld -o $@ $<

examples/%.o: examples/%.S
	as -o $@ $<

examples/%.S: examples/%.bf labrea.scm
	./labrea -fgas -o $@ $<

clean:
	-rm examples/*.o examples/*.S helloworld rot13
