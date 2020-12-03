.PHONY: install clean docs
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

HS_SRC := $(shell find src -type f) kempe.cabal

BINS := bin/x86_64-linux-kc.lz \
    bin/x86_64-linux-kc.gz \
    bin/x86_64-linux-kc.zst

moddeps.svg: $(HS_SRC)
	cd src && graphmod --no-cabal Kempe.File | dot -Tsvg -o ../$@

bins: $(BINS)

docs: docs/manual.pdf docs/manual.html

docs/manual.pdf: docs/manual.md
	pandoc $< -o $@ --toc

docs/manual.html: docs/manual.md
	pandoc -s $< -o $@ --toc

numbertheory.S: lib/numbertheory.kmp
	kc $< --dump-asm > $@

numbertheory.o: numbertheory.S
	nasm -g -f elf64 $< -o $@

numbertheory: numbertheory.o test/harness/numbertheory.c
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
	rm -rf dist-newstyle *.rlib *.d *.rmeta *.o stack.yaml.lock factorial.S factorial splitmix.S numbertheory.S numbertheory *.so bin moddeps.svg

%.zst: %
	sak compress $< $@ --best

%.lz: %
	sak compress $< $@ --best

%.gz: %
	sak compress $< $@ --best

bin/x86_64-linux-kc: $(HS_SRC)
	@mkdir -p $(dir $@)
	cabal build exe:kc --enable-executable-static
	export BIN=$$(fd 'x86_64-linux.*kc$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    strip $@
