(* tests *)

open Format;;

#load "bitv.cmo";;
open Bitv;;
open Bitv.M;;
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
let () = assert (to_string v = "111111111111111000000000001111");;

(* bitwise operations *)
let s = sub v 2 4;;
let () = assert (bw_not (bw_not s) = s);;
let () = assert (bw_and e e = e);;

(* iteri_true *)
let test_iteri_true n =
  let k = 1 + Random.int 5 in
  let v = init n (fun i -> i mod k = 0) in
  iteri_true (fun i -> assert (i mod k = 0)) v

let () =
  for n = 0 to 1700 do test_iteri_true n done

(* shifts *)

let () =
  let v = of_string "110101110" in
  assert (shiftl v 1 = of_string "101011100");
  assert (shiftl v (-1) = of_string "011010111");
  assert (shiftr v 1 = of_string "011010111")

let test_shift n =
  let v = init n (fun _ -> Random.bool ()) in
  let k = Random.int n in
  let w = shiftr v k in
  for i = 0 to n-1-k do assert (get v (k+i) = get w i) done;
  for i = n-k to n-1 do assert (get w i = false) done

let () =
  for n = 1 to 200 do test_shift n done

