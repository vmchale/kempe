# Kempe

Kempe is a stack-based language and toy compiler for x86_64. It requires the
[nasm](https://nasm.us/) assembler.

Inspiration is primarily from [Mirth](https://github.com/mirth-lang/mirth).

See manual
[here](http://hackage.haskell.org/package/kempe/src/doc/manual.pdf).

## Installation

Installation is via [cabal-install](https://www.haskell.org/cabal/):

```
cabal install kempe
```

For shell completions put the following in your `~/.bashrc` or
`~/.bash_profile`:

```
eval "$(kc --bash-completion-script kc)"
```

## Defects

  * Unification takes too long
  * Errors don't have position information
  * Monomorphization fails on recursive polymorphic functions
  * Order of declarations matters past the IR phase (right now mutual recursion
    is broken)
  * If pattern matches fail at runtime, code just keeps running with whatever
    was after the jumps (no pattern match exhaustiveness checker)
  * Can't export or call C functions with more than 6 arguments; can't accept or
    export large arguments (i.e. structs) passed by value.
