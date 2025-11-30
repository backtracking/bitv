
(* sandbox to test performance *)

open Format

let time f x =
  let open Unix in
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  printf "%2.2f@." ut;
  y

open Bitv__Pbv

let () = Random.init 42
let n = int_of_string Sys.argv.(1)

module M = Small(struct let size = Sys.int_size end)
open M
let v = init 63 (fun i -> i < n)
let () = printf "v = %a@." print v
let f v =
  let s = ref 0 in
  iter_subsets (fun v -> s := !s + pop v) v;
  printf "sum = %d@." !s
let () = time f v
