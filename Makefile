.PHONY: install clean docs

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

HS_SRC := $(shell find src -type f) kempe.cabal

BINS := bin/x86_64-linux-kc.lz \
    bin/x86_64-linux-kc.gz \
    bin/x86_64-linux-kc.zst \
    bin/aarch64-linux-kc.lz \
    bin/aarch64-linux-kc.gz \
    bin/aarch64-linux-kc.zst

install:
	cabal install exe:kc --overwrite-policy=always -w ghc-9.2.4
	strip $$(readlink -f $$(which kc))

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

packdeps.svg: kempe.cabal
	cabal build --disable-benchmarks --disable-tests
	cabal-plan dot | dot -Tsvg -o $@

bins: $(BINS)

docs: docs/manual.pdf docs/manual.html

docs/manual.pdf: docs/manual.md
	pandoc $^ -o $@ --toc

docs/manual.html: docs/manual.md
	pandoc -s $^ -o $@ --toc

numbertheory.S: lib/numbertheory.kmp
	kc $^ --dump-asm > $@

numbertheory.o: numbertheory.S
	nasm -g -f elf64 $^ -o $@

numbertheory: numbertheory.o test/harness/numbertheory.c
	gcc -g $^ -o $@

factorial.o: examples/factorial.kmp
	kc -g $^ $@

factorial: factorial.o test/harness/factorial.c
	gcc -g $^ -o $@

rts.o: rts.S
	nasm $^ -f elf64 -o $@

clean:
	rm -rf dist-newstyle *.rlib *.d *.rmeta *.o stack.yaml.lock factorial.S factorial splitmix.S numbertheory.S numbertheory *.so bin moddeps.svg packdeps.svg benchmarks/*_stub.h benchmarks/*.hi benchmarks/*.o

%.zst: %
	sak compress $< $@ --best

%.lz: %
	sak compress $< $@ --best

%.gz: %
	sak compress $^ $@ --best

bin/aarch64-linux-kc: $(HS_SRC)
	@mkdir -p $(dir $@)
	cabal build exe:kc --with-ghc=aarch64-linux-gnu-ghc --with-ghc-pkg=aarch64-linux-gnu-ghc-pkg --constraint='kempe +cross' --enable-executable-static
	export BIN=$$(fd 'aarch64-linux.*kc$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    aarch64-linux-gnu-strip $@

bin/x86_64-linux-kc: $(HS_SRC)
	@mkdir -p $(dir $@)
	cabal build exe:kc
	export BIN=$$(fd 'x86_64-linux.*kc$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    strip $@

tags: $(HS_SRC)
	echo ':ctags' | cabal repl
