(* tests *)

#load "bitv.cmo";;
open Bitv;;
#install_printer print;;

(* 0-length blitting *)
let v = create 30 true;;
let () = blit v 0 v 30 0;;
let () = assert (length v = 30);;

(* 0-length extraction *)
let e = sub v 30 0;; 
let () = assert (length e = 0);;

(* 0-length concatenation *)
let w = append v e;;
let () = assert (length w = 30);;

let w = append e v;;
let () = assert (length w = 30);;

(* filling *)
let () = fill v 4 11 false;;
let () = fill v 30 0 true;;
let () = assert (length v = 30);;
let () = assert (to_string v = "111100000000000111111111111111");;

(* bitwise operations *)
let s = sub v 2 4;;
let () = assert (bw_not (bw_not s) = s);;
let () = assert (bw_and e e = e);;
