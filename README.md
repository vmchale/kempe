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

## Known Bugs & Defects

Right now there is no kind checker so ill-kinded types will not be caught.
