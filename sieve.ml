
open Bitv_string

let primes_upto limit =
  if limit >= max_length then invalid_arg "first_primes_upto";
  let b = create (limit + 1) true in
  unsafe_set b 0 false;
  unsafe_set b 1 false;
  for i = 2 to limit / 2 do unsafe_set b (2 * i) false done;
  let rec loop n =
    if n <= limit then
      if unsafe_get b n then begin (* n is prime *)
	let rec mark i =
	  if i <= limit then begin unsafe_set b i false; mark (i + 2*n) end
	in
	if n <= limit/n then mark (n * n);
	loop (n + 2)
      end else
	loop (n + 2)
  in
  loop 3;
  b

open Format
let limit = int_of_string Sys.argv.(1)
let primes = primes_upto limit
let () = printf "%d primes up to %d@." (pop primes) limit
(* let () = iteri_true (fun i -> printf "%d " i) primes; printf "@." *)

(* benchmarks (original implementation vs string implementation)

5761455 primes up to 100_000_000
  user	0m5.224s
  user	0m1.304s
50847534 primes up to 1_000_000_000
  user	0m54.303s
  user	0m15.065s
*)

(*
Local Variables:
compile-command: "make sieve.opt"
End:
*)
