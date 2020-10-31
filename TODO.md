- [ ] Fix Dickinson modules? lol
- [ ] Take notes on phases, revisit Appel book
# ABI support
- [ ] Kempe ABI proper
- [ ] Fortran?
# Examples
- [ ] delink?
- [ ] Primality test (and link to C...)
  - [ ] Needs sensible float support!
- [x] splitmix random number generator? (or rather any pseudorandom number
  generator...)
  - [ ] http://prng.di.unimi.it/xoroshiro128plus.c
# Performance
## Data Structures
- [ ] Difference lists when laying down atoms/IR
- [ ] Do sets actually help?
# Bugs
- [x] The current setup ignores extern imports -> no it doesn't
- [ ] Exported functions w/ C ABI should be there (so it can link)
# Pipeline
- [ ] FORTRAN backend? :p
- [ ] Inliner (all non-recursive?)
- [ ] IR (abstract assembly?)
  - [ ] https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/13_Assembly1.pdf (von neumann -> c where int ptrs, ATS & such are useful...)
  - [ ] https://www.cs.virginia.edu/~evans/cs216/guides/x86.html
- [ ] http://hackage.haskell.org/package/x86-64bit-0.4.6.1/docs/CodeGen-X86.html
# Features
- [ ] `abstype` for pointer abstract types?
- [ ] REPL for type inspection?
- [ ] Pattern match exhaustiveness checker
- [ ] Tuples (?)
- [ ] Error messages should have line numbers
- [ ] Or-patterns?
- [ ] Tail-call optimization (easier than I thought?)
  - [ ]  Mutually recursive function optimization
- [ ] `x86-64bit` backend? might be useful for REPL :o
## Builtins
- [ ] `rem` builtin or the like?
- [ ] `popcnt` basically
# Test Cases
- [ ] Unit tests for type merge?
- [ ] Unit tests for catTypes?
