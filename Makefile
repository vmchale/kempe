.PHONY: install clean
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

HS_SRC := $(shell find src -type f) kempe.cabal

moddeps.svg: $(HS_SRC)
	graphmod src | dot -Tsvg -o$@

factorial.o: examples/factorial.kmp
	kc $< $@

factorial: rts.o factorial.o test/harness/factorial.c
	gcc $^ -o $@

rts.o: rts.S
	nasm $^ -f elf64 -o $@

install:
	cabal install exe:kc --overwrite-policy=always

clean:
	rm -rf dist-newstyle *.rlib *.d *.rmeta *.o stack.yaml.lock moddeps.svg factorial.S
