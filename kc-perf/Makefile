.PHONY: clean
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

bench: main.dats is_prime.o
	patscc -DATS_MEMALLOC_GCBDW $^ -lgc -o $@

is_prime.o: ../lib/numbertheory.kmp
	kc $< $@

clean:
	rm -rf *_dats.c bench *.o
