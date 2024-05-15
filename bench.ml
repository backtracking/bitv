
(* sandbox to test performance *)

open Bitv

let n = int_of_string Sys.argv.(1)
let v = init n (fun i -> i mod 5 = 0)
let r = ref 0
let add i = r := !r + i
let () = iteri_true add v
let () = Format.printf "%d@." !r


