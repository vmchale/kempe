- [ ] Fix Dickinson modules? lol
- [ ] Take notes on phases, revisit Appel book
# ABI support
- [ ] Kempe ABI proper
- [ ] Fortran?
# Documentation
- [ ] http://texdoc.net/texmf-dist/doc/latex/bytefield/bytefield.pdf
# Examples
- [ ] delink?
- [x] Primality test (and link to C...)
  - [ ] Totient function
  - [ ] Needs sensible float support!
- [x] splitmix random number generator? (or rather any pseudorandom number
  generator...)
  - [ ] http://prng.di.unimi.it/xoroshiro128plus.c
- [ ] Exponentiation (O (log n))
# Backends
- [x] Linear scan register allocator
- [ ] Graph (?) register allocator
# Code
- [ ] `typed-process`?
# Performance
- [ ] DList for arm backend? laying down asm...
- [ ] look at http://hackage.haskell.org/package/linearscan-hoopl
- [ ] Liveness analysis on a per-decl basis? (basic blocks)
- [ ] `lea`, `xchg`? `test`?
- [ ] `11111` could be True, everything else is false (then could use `xor` for
  int eq?) -> alternately `8` could be `True`, all else false `xor + popcnt`?
- [ ] https://hackage.haskell.org/package/hashtables
## Data Structures
- [ ] Difference lists when laying down atoms/IR
- [ ] Do sets actually help?
  - [ ] https://hackage.haskell.org/package/hoopl (may be faster)
- [ ] https://hackage.haskell.org/package/fmlist
# Bugs
- [x] The current setup ignores extern imports -> no it doesn't
- [x] Exported functions w/ C ABI should be there (so it can link)
- [ ] Throw error when return value in C ABI is too big
- [x] Constructors aren't monomorphized
  - [ ] http://mlton.org/Monomorphise
  - [x] Constructors that call constructors don't dispatch/monomorphize
    properly?
- [x] Correctly restore registers (C ABI)
- [ ] Warn on >256 constructors
- [x] Constructors not inlined; need type specializations when one calls
  a constructor on a constructor!
- [x] Save callee-save registers on C call
  - [ ] caller-save registers (`popa`?)
# Pipeline
- [x] Inliner (all non-recursive?)
- [ ] https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/13_Assembly1.pdf (von neumann -> c where int ptrs, ATS & such are useful...)
# Features
- [ ] Only save registers that get clobbered
- [ ] Aarch64 backend
- [ ] Arithmetic and patterns for `i8`
- [ ] Patterns for words?
- [ ] `divMod` builtin?
- [ ] Floats
- [ ] `fmt` subcommand
- [ ] `abstype` for pointer abstract types?
  - [ ] maybe just builtin lol
- [ ] convert ints &c. between types
- [ ] REPL for type inspection?
  - [ ] ─
- [x] Pattern match exhaustiveness checker
- [ ] Tuples (?)
- [ ] Error messages should have line numbers
- [x] Tail-call optimization (easier than I thought?)
  - [x]  Mutually recursive function optimization (what ATS does?)
- [ ] tail recursion modulo cons ?
- [ ] `.intel_syntax noprefix` for arm
- [ ] RCL/RCR/ROL/ROR
## Builtins
- [ ] `rem` builtin or the like? (basically functions in library)
  - [ ] `sal`, `sar`?
- [x] `popcnt` basically
# Test Cases
- [ ] Unit tests for type merge?
- [ ] Unit tests for catTypes?
- [ ] Test foreign calls (e.g. `random`)
