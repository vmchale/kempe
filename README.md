# kempe

Kempe is a stack-based language and toy compiler for x86_64. It requires the
[nasm](https://nasm.us/) assembler.

Inspiration is primarily from [Mirth](https://github.com/mirth-lang/mirth).

## Installation

For shell completions but the following in your `~/.bashrc` or
`~/.bash_profile`:

```
eval "$(kc --bash-completion-script kc)"
```

## Defects

  * Unification takes too long
  * Errors don't have position information
  * Monomorphization fails on recursive polymorphic functions.
  * Order of declarations matters past the IR phase
