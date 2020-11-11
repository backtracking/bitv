
(* Comparing Bitv and Bitv_string on a simple Eratosthene's sieve *)

open Bitv

let sieve1 limit =
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

let () = assert (pop (sieve1 100) = 25)

open Bitv_string

let sieve2 limit =
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

let () = assert (pop (sieve2 100) = 25)

open Format
let () = match Sys.argv with
  | [| _; a; n |] ->
      let n = int_of_string n in
      let p =
        if a = "1" then Bitv.pop (sieve1 n) else Bitv_string.pop (sieve2 n ) in
      printf "%d primes up to %d@." p n
  | _ -> ()

(* benchmarks

+------------+----------+------------+------------+------------+
|          N |     10^8 |       10^9 |     2.10^9 |     3.10^9 |
+------------+----------+------------+------------+------------+
| Bitv       |      1.6 |       18.2 |       37.9 |       57.7 |
+------------+----------+------------+------------+------------+
| Bitv_string|      1.3 |       15.1 |       31.3 |       47.7 |
+------------+----------+------------+------------+------------+
|#primes     |  5761455 |   50847534 |   98222287 |  144449537 |
+------------+----------+------------+------------+------------+

*)
