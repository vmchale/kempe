- [ ] Fix Dickinson modules? lol
- [ ] Take notes on phases, revisit Appel book
# ABI support
- [ ] Kempe ABI proper
- [ ] Fortran?
# Examples
- [ ] delink?
- [ ] Primality test (and link to C...)
  - [ ] Totient function
  - [ ] Needs sensible float support!
- [x] splitmix random number generator? (or rather any pseudorandom number
  generator...)
  - [ ] http://prng.di.unimi.it/xoroshiro128plus.c
# Backends
- [ ] Linear scan register allocator
- [ ] Graph (?) register allocator
# Performance
- [ ] Liveness analysis on a per-decl basis?
## Data Structures
- [ ] Difference lists when laying down atoms/IR
- [ ] Do sets actually help?
  - [ ] https://hackage.haskell.org/package/hoopl (may be faster)
# Bugs
- [x] The current setup ignores extern imports -> no it doesn't
- [x] Exported functions w/ C ABI should be there (so it can link)
- [ ] Throw error when return value in C ABI is too big
- [ ] Constructors aren't monomorphized
- [ ] Correctly restore registers (C ABI)
# Pipeline
- [ ] Inliner (all non-recursive?)
- [ ] https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/13_Assembly1.pdf (von neumann -> c where int ptrs, ATS & such are useful...)
# Features
- [ ] Float ABI support
- [ ] `fmt` subcommand
- [ ] `abstype` for pointer abstract types?
- [ ] REPL for type inspection?
  - [ ] ─
- [ ] Pattern match exhaustiveness checker
- [ ] Tuples (?)
- [ ] Error messages should have line numbers
- [ ] Or-patterns?
- [ ] Tail-call optimization (easier than I thought?)
  - [ ]  Mutually recursive function optimization
- [ ] tail recursion modulo cons ?
- [ ] `.intel_syntax noprefix` for arm
## Builtins
- [ ] `rem` builtin or the like?
  - [ ] `sal`, `sar`?
- [ ] `popcnt` basically
# Test Cases
- [ ] Unit tests for type merge?
- [ ] Unit tests for catTypes?
