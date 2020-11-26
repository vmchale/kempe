% Kempe Compiler & Language Manual
% Vanessa McHale

# Introduction

Kempe is a stack-based language, and `kc` is a toy compiler for x86_64.

# Installing kc

First, install [cabal](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html). Then:

```
cabal install kempe
```

This provides `kc`, the Kempe compiler.

## Editor Integration

A [vim plugin](https://github.com/vmchale/kempe/tree/master/vim) is
available.

To install with [vim-plug](https://github.com/junegunn/vim-plug):

```vimscript
Plug 'vmchale/kempe' , { 'rtp' : 'vim' }
```

# Kempe Language

## Types

Kempe has a stack-based type system. So if you see a type signature:

```
next : Word -- Word Word
```

that means that the stack must have a `Word` on it for `next` to be invoked, and
that it will have two `Word`s on the stack after it is invoked.

### Polymorphism

Kempe allows polymorphic functions. So we can define:

```
id : a -- a
   =: [ ]
```

## Builtins

The Kempe compiler has a few builtin functions that you can use for arithmetic
and for shuffling data around. Many of them are familiar to stack-based
programmers:

  * `dup : a -- a a`
  * `swap : a b -- b a`

There is one higher-order construct, `dip` - consider an example:

```
nip : a b -- b
    =: [ dip(drop) ]
```

# Programming in Kempe

## Invoking the Compiler

`kc` cannot be used to produce executables. Rather, the Kempe compiler will
produce `.o` files which contain functions.

# Examples

## Splitmix Pseudorandom Number Generator

The generator in question comes from a [recent
paper](https://dl.acm.org/doi/10.1145/2714064.2660195).

Implementation turns out to be quite nice thanks to Kempe's multiple return
values:

```
; given a seed, return a random value and the new seed
next : Word -- Word Word
     =: [ 0x9e3779b97f4a7c15u +~ dup
          dup 30i8 >>~ xoru 0xbf58476d1ce4e5b9u *~
          dup 27i8 >>~ xoru 0x94d049bb133111ebu *~
          dup 31i8 >>~ xoru
        ]

%foreign kabi next
```

Compare the [C implementation](http://prng.di.unimi.it/splitmix64.c):

```c
#include <stdint.h>

// modified to have ""multiple return"" since C doesn't really have that
uint64_t next(uint64_t x, uint64_t* y) {
	uint64_t z = (x += 0x9e3779b97f4a7c15);
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
	z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
	*y = x;
	return z ^ (z >> 31);
}
```
