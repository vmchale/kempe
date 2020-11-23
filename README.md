# kempe

Kempe is a stack-based language and compiler for x86_64. It requires the
[nasm](https://nasm.us/) assembler.

Inspiration is primarily from [Mirth](https://github.com/mirth-lang/mirth).

## Installation

For shell completions but the following in your `~/.bashrc` or
`~/.bash_profile`:

```
eval "$(kc --bash-completion-script kc)"
```

## Defects

  * Rather than an instruction selection phase, the compiler translates
    to assembly directly.
  * Unification takes too long
  * Doesn't actually save callee-save registers
  * Sum types aren't translated in the IR phase
