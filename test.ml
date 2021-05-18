(* tests *)

open Bitv
open Bitv.M

(* 0-length blitting *)
let v = create 30 true
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
let () = assert (String.equal (to_string v) "111111111111111000000000001111")

(* bitwise operations *)
let s = sub v 2 4
let () = assert (equal (bw_not (bw_not s)) s)
let () = assert (equal (bw_and e e) e)

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

module type IntLike = sig
  type t
  val equal : t -> t -> bool
  val add : t -> t -> t
  val min_int : t

  val size_signed : int
  val random_unsigned : unit -> t

  val of_unsigned : t -> Bitv.t
  val of_signed : t -> Bitv.t
  val to_unsigned : Bitv.t -> t
  val to_signed : Bitv.t -> t
end

module type Gen = sig
  type t
  val equal : t -> t -> bool
  val size : int
  val random : unit -> t
  val of_ : t -> Bitv.t
  val to_ : Bitv.t -> t
end

module UnsignedGen (I : IntLike) : Gen =
struct
  include I
  let size = size_signed - 1
  let random = random_unsigned
  let of_ = of_unsigned
  let to_ = to_unsigned
end

module SignedGen (I : IntLike) : Gen =
struct
  include I
  let size = size_signed
  let random () = add min_int (add (random_unsigned ()) (random_unsigned ()))
  let of_ = of_signed
  let to_ = to_signed
end

let test_conv (gen : (module Gen)) =
  let open (val gen) in
  let test x =
    let v = of_ x in
    assert (length v = size);
    assert (equal (to_ v) x)
  in
  for _k = 1 to 1000 do test (random ()) done

module Int : IntLike with type t = int = struct
  (* The following four lines should be replaced by [include Int] once
     we require OCaml 4.08 or newer *)
  type t = int
  let equal : int -> int -> bool = (=)
  let add = (+)
  let min_int = min_int

  let size_signed = Sys.int_size
  (* [Random.int] and [Random.bits] do not generate anything above 2^30.
     Until ocaml/ocaml#9489 is resolved, the easiest workaround seems to
     be using [Random.int64]. It is also unclear whether we want to depend
     on OCaml 4.13+ just for the new generator for [int]. *)
  let () = assert (Sys.int_size <= 64) (* would fail when 64bit -> 128bit *)
  let random_unsigned () = Int64.to_int (Random.int64 (Int64.of_int max_int))
  let of_unsigned = of_int_us
  let to_unsigned = to_int_us
  let of_signed = of_int_s
  let to_signed = to_int_s
end
let () = test_conv (module UnsignedGen(Int))
let () = test_conv (module SignedGen(Int))

module Int32 : IntLike with type t = int32 = struct
  include Int32
  let size_signed = 32
  let random_unsigned () = Random.int32 max_int
  let of_unsigned = of_int32_us
  let to_unsigned = to_int32_us
  let of_signed = of_int32_s
  let to_signed = to_int32_s
end
let () = test_conv (module UnsignedGen(Int32))
let () = test_conv (module SignedGen(Int32))

module Int64 : IntLike with type t = int64 = struct
  include Int64
  let size_signed = 64
  let random_unsigned () = Random.int64 max_int
  let of_unsigned = of_int64_us
  let to_unsigned = to_int64_us
  let of_signed = of_int64_s
  let to_signed = to_int64_s
end
let () = test_conv (module UnsignedGen(Int64))
let () = test_conv (module SignedGen(Int64))

module Nativeint : IntLike with type t = nativeint = struct
  include Nativeint
  let size_signed = Sys.word_size
  let random_unsigned () = Random.nativeint max_int
  let of_unsigned = of_nativeint_us
  let to_unsigned = to_nativeint_us
  let of_signed = of_nativeint_s
  let to_signed = to_nativeint_s
end
let () = test_conv (module UnsignedGen(Nativeint))
let () = test_conv (module SignedGen(Nativeint))

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

open Bitv_string

(* 0-length blitting *)
let v = create 30 true
let () = assert (length v = 30)
let () = assert (get v 17)

(* 0-length extraction *)
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
