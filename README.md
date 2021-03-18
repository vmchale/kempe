# Kempe

Kempe is a stack-based language and toy compiler for x86_64 and aarch64. It requires the
[nasm](https://nasm.us/) assembler when targeting x86_64.

Inspiration is primarily from [Mirth](https://github.com/mirth-lang/mirth).

See manual
[here](http://hackage.haskell.org/package/kempe/src/docs/manual.pdf).

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

  * Errors don't have position information
  * Monomorphization fails on recursive polymorphic functions

    Hopefully this isn't too sinful; I can't think of any examples of recursive
    polymorphic functions
  * Can't export or call C functions with more than 6 arguments; can't call or
    export large arguments (i.e. structs) passed by value.

    This is less of an impediment than it sounds like.
  * Cyclic imports are not detected
  * Imports are kind of defective

### Comparison

You may wish to use [Mirth](https://github.com/mirth-lang/mirth/) or
[Factor](https://factorcode.org/) instead. Mirth is statically typed (similar to
Kempe) but less mature.
