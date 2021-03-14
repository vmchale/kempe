# kempe

  * Add aarch64 backend

## 0.1.1.3

  * Tweak some RTS flags for faster performance
  * `lib/gaussian.kmp` has `mult` function
  * A couple inefficiencies under the hood

## 0.1.1.2

  * Case statements with a single branch are plain and efficient.
  * Add `safeDiv` and `safeMod` to `prelude/arith.kmp`

## 0.1.1.1

  * Performance improvements under the hood (use `IntSet`s for liveness
    analysis)

## 0.1.1.0

  * Fix internal pretty-printer (exposed as hidden `fmt` subcommand)
  * Optimize IR cases
  * Fix padding
  * Fix bug in lexer (for C foreign calls)
  * Support down to GHC 8.0.2
  * Unification no longer takes pathologically long time
  * Add test files so source distribution passes
  * Some sort of imports now supported.

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
