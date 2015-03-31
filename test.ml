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

(* conversions to/from integers *)

let test_conv size random fto fof =
  let test x =
    let v = fof x in
    assert (length v = size);
    assert (fto v = x)
  in
  for k = 1 to 1000 do test (random ()) done

let () = test_conv (Sys.word_size-2) Random.bits to_int_us of_int_us
let random_int_s () = min_int + (Random.bits ()) + (Random.bits ())
let () = test_conv (Sys.word_size-1) random_int_s to_int_s  of_int_s

let random_int32_us () = Random.int32 Int32.max_int
let () = test_conv 31 random_int32_us to_int32_us of_int32_us
let random_int32_s () =
  Int32.add Int32.min_int (Int32.add (random_int32_us ()) (random_int32_us ()))
let () = test_conv 32 random_int32_s  to_int32_s of_int32_s

let random_int64_us () = Random.int64 Int64.max_int
let () = test_conv 63 random_int64_us to_int64_us of_int64_us
let random_int64_s () =
  Int64.add Int64.min_int (Int64.add (random_int64_us ()) (random_int64_us ()))
let () = test_conv 64 random_int64_s  to_int64_s of_int64_s

let random_native_us () = Random.nativeint Nativeint.max_int
let random_native_s () =
  Nativeint.add Nativeint.min_int
    (Nativeint.add (random_native_us ()) (random_native_us ()))
let () = test_conv Sys.word_size random_native_s  to_nativeint_s of_nativeint_s
let () =
  test_conv (Sys.word_size-1) random_native_us to_nativeint_us of_nativeint_us

(* input/output *)

let test_io v =
  let f = Filename.temp_file "bv" "" in
  let c = open_out f in
  output_bin c v;
  close_out c;
  let c = open_in f in
  let w = input_bin c in
  close_in c;
  try Sys.remove f with _ -> ();
  assert (v = w)

let () =
  for n = 0 to 200 do test_io (init n (fun _ -> Random.bool ())) done
