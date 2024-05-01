(* tests *)

open Bitv
open Bitv.M

let () =
  let test len b =
    let v = create len b in
    assert (length v = len);
    assert (if b then all_ones v else all_zeros v);
    assert (bw_and v v = v);
    assert (bw_or v v = v);
    assert (bw_not (bw_not v) = v);
  in
  for len = 0 to 100 do test len false; test len true done

(* 0-length blitting *)
let v = create 30 true
let () = assert (all_ones v)
let () = blit v 0 v 30 0
let () = assert (length v = 30)

(* 0-length extraction *)
let e = sub v 30 0
let () = assert (length e = 0)
let () = assert (equal e (Bitv.create 0 true))

(* 0-length concatenation *)
let w = append v e
let () = assert (length w = 30)
let () = assert (equal w v)

let w = append e v
let () = assert (length w = 30)
let () = assert (equal w v)

(* filling *)
let () = fill v 4 11 false
let () = fill v 30 0 true
let () = assert (length v = 30)
let () = assert (to_string v = "111111111111111000000000001111")

(* bitwise operations *)
let s = sub v 2 4
let () = assert (equal (bw_not (bw_not s)) s)
let () = assert (equal (bw_and e e) e)

(* Tanimoto score *)
let () =
  let b0 = create 10 false in
  let b1 = create 10 true in
  let even = init 10 (fun i -> i mod 2 = 0) in
  let odd = init 10 (fun i -> i mod 2 = 1) in
  assert (tanimoto b0 b1 = 0.);
  assert (tanimoto b1 b1 = 1.);
  assert (compare (tanimoto b0 b0) nan = 0);
  assert (tanimoto even b1 = 0.5);
  assert (tanimoto odd b1 = 0.5)

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
  assert (equal (shiftl v 1) (of_string "101011100"));
  assert (equal (shiftl v (-1)) (of_string "011010111"));
  assert (equal (shiftr v 1) (of_string "011010111"))

let test_shift n =
  let v = init n (fun _ -> Random.bool ()) in
  let k = Random.int n in
  let w = shiftr v k in
  for i = 0 to n-1-k do assert (get v (k+i) = get w i) done;
  for i = n-k to n-1 do assert (get w i = false) done

let () =
  for n = 1 to 200 do test_shift n done

(* rotations *)

let () =
  let v = of_string "110101110" in
  assert (equal (rotatel v 1) (of_string "101011101"));
  assert (equal (rotatel v (-1)) (of_string "011010111"));
  assert (equal (rotater v 1) (of_string "011010111"))

let test_rotate n =
  let v = init n (fun _ -> Random.bool ()) in
  let k = Random.int (2*n) - n in
  let w = rotatel v k in
  for i = 0 to n-1 do assert (get v i = get w ((i + n + k) mod n)) done

let () =
  for n = 1 to 200 do test_rotate n done

(* conversions to/from lists *)

let () =
  let test len l =
    let v = of_list l in
    assert (length v = len);
    assert (to_list v = l);
    assert (v = of_list_with_length l len)
  in
  test 0 [];
  test 42 [41];
  test 35 [0;1;2;3;5;8;13;21;34]

(* conversions to/from strings *)

let () =
  let bits0 = "1000011001000000000000000000000100000001110000000000000000000000000000000000011100000000110111000000000000000000000000000000000000000111011100"
  and bits1 = "1000011001000000000000000000000100000001110000000000000000000111100000000000000000000000110111000000000000000000000000000000000000000111011100"
  and zeros = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  in
  assert (equal (of_string "1") (of_string "1"));
  assert (equal (of_string bits0) (of_string bits0));
  assert (equal (of_string bits1) (of_string bits1));
  assert (not (equal (of_string "") (of_string "0")));
  assert (not (equal (of_string "1") (of_string "01")));
  assert (not (equal (of_string "1") (of_string "10")));
  assert (not (equal (of_string bits0) (of_string bits1)));
  assert (not (equal (of_string bits1) (of_string bits0)));
  assert (String.equal (to_string (of_string "")) "");
  assert (String.equal (to_string (of_string "0")) "0");
  assert (String.equal (to_string (of_string zeros)) zeros);
  assert (String.equal (to_string (of_string bits0)) bits0);
  assert (String.equal (to_string (of_string bits1)) bits1)

(* conversions to/from integers *)

let test_conv size random fto fof =
  let test x =
    let v = fof x in
    assert (length v = size);
    assert (fto v = x)
  in
  for _k = 1 to 1000 do test (random ()) done

(***
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
  let f = Filename.temp_file "bitv" "" in
  let c = open_out f in
  output_bin c v;
  close_out c;
  let c = open_in f in
  let w = input_bin c in
  close_in c;
  begin try Sys.remove f with _ -> () end;
  assert (equal v w)

let test_bytes v =
  assert (equal (of_bytes (to_bytes v)) v)

let test_equivalent v =
  let f = Filename.temp_file "bitv" "" in
  let c = open_out f in
  output_bin c v;
  close_out c;
  let c = open_in f in
  let len = in_channel_length c in
  let b = Bytes.create len in
  really_input c b 0 len;
  close_in c;
  begin try Sys.remove f with _ -> () end;
  assert (Bytes.equal b (to_bytes v))

let () =
  for n = 0 to 200 do
    let bv = init n (fun _ -> Random.bool ()) in
    test_io bv;
    test_bytes bv;
    test_equivalent bv;
  done
***)

let v = create 30 true
let () = assert (length v = 30)
let () = assert (get v 17)

let e = create 0 false
let () = assert (length e = 0)

(* filling *)
let () = fill v 4 11 false
let () = fill v 30 0 true
let () = assert (length v = 30)
let () = assert (pop v = 19)

let ones = create 30 true
let () = assert (pop ones = 30)
let zeros = create 30 false
let () = assert (pop zeros = 0)
let () = assert (equal (bw_or v ones) ones)
let () = assert (equal (bw_and v ones) v)
let () = assert (equal (bw_xor v zeros) v)
let () = assert (equal (bw_xor v ones) (bw_not v))
