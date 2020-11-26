staload "libats/ML/SATS/atspre.sats"
staload "libats/ML/SATS/string.sats"
staload UN = "prelude/SATS/unsafe.sats"

#define :: list_vt_cons
#define nil list_vt_nil

extern
fun start_timer() : void =
  "ext#"

extern
fun microseconds() : int =
  "ext#"

extern
fun dup(int) : int =
  "ext#"

%{
#include <sys/time.h>
#include <time.h>

struct timeval timer;

void start_timer() { gettimeofday(&timer, NULL); }

int microseconds() {
  struct timeval now;
  gettimeofday(&now, NULL);
  int secs = now.tv_sec - timer.tv_sec;
  int us = now.tv_usec - timer.tv_usec;
  return (1000000 * secs + us);
}
%}

typedef io = () -> void

fun display_time(x : double) : void =
  ifcase
    | x >= 1000000 => println!(x / 1000000, " s")
    | x >= 1000 => println!(x / 1000, " ms")
    | x >= 1 => println!(x, " μs")
    | _ => println!(x * 1000, " ns")

fun bench_f(n : intGt(0), x : io) : int =
  let
    var before = start_timer()
    
    fun loop { n : nat | n > 0 } .<n>. (n : int(n), x : io) : void =
      case+ n of
        | 1 => x()
        | m =>> (x() ; loop(m - 1, x))
    
    val _ = loop(n, x)
  in
    microseconds()
  end

fun create_entry(n : int, x : io) : double =
  let
    var pre_d = bench_f($UN.cast(n), x)
  in
    gnumber_int<double>(pre_d)
  end

typedef regression = @{ intercept = double, slope = double }
typedef pair = @{ x = double, y = double }

fun sum(xs : !List_vt(double)) : double =
  list_vt_foldleft_cloptr<double>(xs, 0.0, lam (acc, next) =<cloptr1> acc + next)

// TODO collect more data here (e.g. r^2, etc.)
fn regress(pairs : List_vt(pair)) : regression =
  let
    var n = list_vt_length(pairs)
    var ys = list_vt_map_cloref(pairs, lam p =<cloref1> p.y)
    var xs = list_vt_map_cloref(pairs, lam p =<cloref1> p.x)
    var xys = list_vt_map_cloref(pairs, lam p =<cloref1> p.x * p.y)
    var xxs = list_vt_mapfree_cloref(pairs, lam p =<cloref1> p.x * p.x)
    var sigma_y = sum(ys)
    var sigma_x = sum(xs)
    var sigma_xy = sum(xys)
    var sigma_xx = sum(xxs)
    val _ = list_vt_free(ys)
    val _ = list_vt_free(xs)
    val _ = list_vt_free(xys)
    val _ = list_vt_free(xxs)
    var denom = (n * sigma_xx - sigma_x * sigma_x)
    var intercept = (sigma_y * sigma_xx - sigma_x * sigma_xy) / denom
    var slope = (n * sigma_xy - sigma_x * sigma_y) / denom
  in
    @{ intercept = intercept, slope = slope }
  end

fun iota {n:nat} .<n>. (i : int(n)) : list_vt(int, n) =
  case+ i of
    | 0 => nil
    | n =>> (n - 1) :: iota(n - 1)

fn create_pairs {n:nat}(n : int(n), d : io) : List_vt(pair) =
  let
    var pre_seq = iota(n)
    var correct = list_vt_mapfree_cloref(pre_seq, lam n =<cloref1> (3 ** $UN.cast(n)))
    var pairs = list_vt_mapfree_cloref(correct, lam n =<cloref1> let
                                        var nd = gnumber_int<double>(n)
                                      in
                                        @{ x = nd, y = create_entry($UN.cast(n), d) }
                                      end)
  in
    pairs
  end

// TODO figure out pretty display for it all?
fn get_slope {n:nat}(n : int(n), d : io) : double =
  let
    var pairs = regress(create_pairs(n, d))
  in
    pairs.slope
  end

fn print_slope {n:nat}(s : string, n : int(n), d : io) : void =
  {
    val sl = get_slope(n, d)
    val _ = print("\33[32m" + s + "\33[0m\n    estimate: ")
    val _ = display_time(sl)
  }
