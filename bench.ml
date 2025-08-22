
(* sandbox to test performance *)

open Format

let time f x =
  let open Unix in
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  printf "%2.2f@." ut;
  y

open Bitv

let () = Random.init 42
let n = int_of_string Sys.argv.(1)
let v = random n
let () = printf "length = %d@." (length v)
let () = if n < 200 then (
           printf "%a@." M.print v;
           let p = fold_left (fun p b -> if b then p+1 else p) 0 v in
           printf "%d@." p
         )
let () = printf "%d@." (time pop v)
(* let () = printf "%d@." (time pop_fast v) *)
