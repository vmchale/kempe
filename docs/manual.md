% Kempe Compiler & Language Manual
% Vanessa McHale

# Introduction

Kempe is a stack-based language, and `kc` is a compiler for x86_64.

# Installing kc

## Source

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

# Programming in Kempe

## Types

Kempe has a stack-based type system.

## Invoking the Compiler

Kempe has few builtins.

# Examples
