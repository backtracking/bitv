
(* sandbox to test performance *)

open Bitv

let () = Random.self_init ()
let n = int_of_string Sys.argv.(1)
(* let v = init n (fun _ -> Random.bool ()) *)
let v = random n (* much faster *)


