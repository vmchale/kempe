#include "share/HATS/atspre_staload_prelude.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
#include "./bench.dats"

extern fn is_prime(int) : bool = "ext#"

fn is_prime_bench() : void =
  {
    val i = is_prime(2017)
  }

val is_prime_delay: io = lam () => is_prime_bench()

implement main0 () =
  {
    val () = print_slope("is_prime", 15, is_prime_delay)
  }

