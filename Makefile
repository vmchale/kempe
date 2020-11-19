.PHONY: install clean
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

HS_SRC := $(shell find src -type f) kempe.cabal

moddeps.svg: $(HS_SRC)
	graphmod src | dot -Tsvg -o$@

splitmix.o: examples/splitmix.kmp
	kc -g $< $@

splitmix: splitmix.o test/harness/splitmix.c
	gcc -g $^ -o $@

factorial.o: examples/factorial.kmp
	kc -g $< $@

factorial: factorial.o test/harness/factorial.c
	gcc -g $^ -o $@

rts.o: rts.S
	nasm $^ -f elf64 -o $@

install:
	cabal install exe:kc --overwrite-policy=always

clean:
	rm -rf dist-newstyle *.rlib *.d *.rmeta *.o stack.yaml.lock moddeps.svg factorial.S factorial
