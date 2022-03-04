# kempe

# 0.2.0.12

  * Typechecker is 𝜖 faster
  
# 0.2.0.11

  * Fix bug in typechecker

## 0.2.0.10

  * Fix bug in typechecking against inferred signatures.
  * Fix bug in prelude

## 0.2.0.9

  * Add `armabi` method of exporting Kempe functions, so that `kc` generates
    suitable code for M1 darwin.
  * Add `cdecl` subcommand to generate C headers for exported functions

## 0.2.0.8

  * More efficient IR generation; don't copy bytes from a source to the same
    destination
  * Fix bugs in `case` IR generation

## 0.2.0.7

  * Fix bug in unification
  * Fix bug so that `and` and `or` instructions print correctly for x86 assembler
  * Add lints for `dip(+) +` to `+ +`, (associative dip) `dup and` to `id`, etc.
  * Add lints for `swap swap` and `1 drop` etc.
  * Add `join` for `Maybe` and `Either`
  * Last branch of case statement always falls through (more efficient code)

## 0.2.0.6

  * Add `absInt` and `chocie` functions to prelude.
  * Add lints for `swap >`, `swap *` &c.
  * Fix bug in typing `>=`, `>`, `!=`

## 0.2.0.5

  * Fix bug in arm control-flow analysis
  * Fix bugs in IR optimization pass
  * Improve IR optimization
  * Add `fromMaybe` and `fromRight` functions

## 0.2.0.4

  * Kind-check external function declarations
  * Fix bug in inliner where functions within `dip(...)`s were not inlined
  * Fix unification bug where solved constraints were not back-substituted correctly.

## 0.2.0.3

  * GHC 8.0.2 and 8.2.2

## 0.2.0.2

  * Improve performance + generated code
  * Fix bug in monomorphization of patterns

## 0.2.0.1

  * Performance improvements when assembling x86
  * Fix pattern match exhaustiveness checker
  * More lenient command-line parser

## 0.2.0.0

  * Add aarch64 backend
  * Change type of shifts, they no longer take an `Int8` as the second argument.

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
