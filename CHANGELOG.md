# kempe

  * Fix internal pretty-printer (exposed as hidden `fmt` subcommand)
  * Optimize IR cases
  * Fix padding
  * Fix bug in lexer (for C foreign calls)
  * Support down to GHC 7.10.3
  * Unification no longer takes pathologically long time
  * Add test files so source distribution passes

## 0.1.0.2

  * Add optimizations (simplify code so that liveness analysis is quicker)
  * Fix major bug in kind-checker
  * Fix bug in type assignment

## 0.1.0.1

  * Better debug pretty-printer
  * Pattern match exhaustiveness checker so that pattern matches don't do
    something heinous at runtime

## 0.1.0.0

Initial release
