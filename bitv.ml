(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Bit vectors. The interface and part of the code are borrowed from the
   [Array] module of the OCaml standard library (but things are simplified
   here since we can always initialize a bit vector). This module also
   provides bitwise operations. *)

(* We represent a bit vector by a string of bytes (field [bits]), and
   we keep the information of the size of the bit vector (field
   [length]) since it can not be figured out with the size of the
   array. *)

let[@inline] byte s i = Char.code (Bytes.unsafe_get s i)

let set_byte s i x = Bytes.unsafe_set s i (Char.unsafe_chr x)

type t = {
  length : int;
  bits   : Bytes.t }
  (* invariant: the unused high bits of the last byte are zeros (if any) *)

let length v = v.length

let[@inline] equal (v1: t) (v2: t) = v1 = v2

let max_length =
  (* prevent overflow with 32-bit integers on JavaScript *)
  if max_int lsr 3 < Sys.max_string_length then max_int else
  Sys.max_string_length * 8

let exceeds_max_length n =
  let s = n / 8 in
  (if n mod 8 = 0 then s else s + 1) > Sys.max_string_length

let low_mask = Array.init 9 (fun i -> (1 lsl i) - 1)

let create n b =
  if n < 0 || n > max_length then invalid_arg "Bitv.create";
  let initv = if b then 255 else 0 in
  let q = n lsr 3 in
  let r = n land 7 in
  if r = 0 then
    { length = n; bits = Bytes.make q (Char.chr initv) }
  else begin
    let s = Bytes.make (q + 1) (Char.chr initv) in
    set_byte s q (initv land low_mask.(r));
    { length = n; bits = s }
  end

let normalize v =
  let r = v.length land 7 in
  if r > 0 then
    let b = v.bits in
    let s = Bytes.length b in
    set_byte b (s-1) ((byte b (s-1)) land low_mask.(r))

let copy v = { length = v.length; bits = Bytes.copy v.bits }

let unsafe_get v n =
  let i = n lsr 3 in
  (byte v.bits i) land (1 lsl (n land 7)) > 0

let get v n =
  if n < 0 || n >= v.length then invalid_arg "Bitv.get";
  unsafe_get v n

let unsafe_set v n b =
  let i = n lsr 3 in
  let c = byte v.bits i in
  let mask = 1 lsl (n land 7) in
  set_byte v.bits i (if b then c lor mask else c land (lnot mask))

let set v n b =
  if n < 0 || n >= v.length then invalid_arg "Bitv.set";
  unsafe_set v n b

(* [init] is implemented naively using [unsafe_set]. *)

let init n f =
  let v = create n false in
  for i = 0 to pred n do unsafe_set v i (f i) done;
  v

let random n =
  let v = create n false in
  let b = v.bits in
  let n = Bytes.length b in
  for i = 0 to n / 3 do let j = 3 * i in
    let bits = Random.bits () in
    set_byte b j     (bits          land 0xFF);
    set_byte b (j+1) ((bits lsr  8) land 0xFF);
    set_byte b (j+2) ((bits lsr 16) land 0xFF)
  done;
  for i = 3 * (n / 3) to n - 1 do set_byte b i (Random.int 256) done;
  normalize v;
  v

let fill v ofs len b =
  if ofs < 0 || len < 0 || ofs > v.length - len then invalid_arg "Bitv.fill";
  if len > 0 then (
  (* incomplete first byte, if any (8-r bits) *)
  let r = ofs land 7 in
  let first = if r = 0 then 0 else min len (8-r) in
  for i = ofs to ofs + first-1 do unsafe_set v i b done;
  (* full bytes in the middle *)
  let start = (ofs + first) lsr 3 in
  let n = (len - first) lsr 3 in
  let x = Char.chr (if b then 0xFF else 0) in
  for i = start to start + n - 1 do Bytes.unsafe_set v.bits i x done;
  (* incomplete last byte, if any *)
  let s = (len - first) land 7 in
  if s > 0 then
    let stop = ofs + len in
    for i = stop - s to stop - 1 do unsafe_set v i b done
  )

(* All the iterators are implemented as for traditional arrays, using
   [unsafe_get]. For [iter] and [map], we do not precompute [(f
   true)] and [(f false)] since [f] may have side-effects. *)

let iter f v =
  for i = 0 to v.length - 1 do f (unsafe_get v i) done

let map f v =
  let l = v.length in
  let r = create l false in
  for i = 0 to l - 1 do
    unsafe_set r i (f (unsafe_get v i))
  done;
  r

let iteri f v =
  for i = 0 to v.length - 1 do f i (unsafe_get v i) done

let mapi f v =
  let l = v.length in
  let r = create l false in
  for i = 0 to l - 1 do
    unsafe_set r i (f i (unsafe_get v i))
  done;
  r

let fold_left f x v =
  let r = ref x in
  for i = 0 to v.length - 1 do
    r := f !r (unsafe_get v i)
  done;
  !r

let fold_right f v x =
  let r = ref x in
  for i = v.length - 1 downto 0 do
    r := f (unsafe_get v i) !r
  done;
  !r

let foldi_left f x v =
  let r = ref x in
  for i = 0 to v.length - 1 do
    r := f !r i (unsafe_get v i)
  done;
  !r

let foldi_right f v x =
  let r = ref x in
  for i = v.length - 1 downto 0 do
    r := f i (unsafe_get v i) !r
  done;
  !r

(*
let iteri_true f v =
  Bytes.iteri
    (fun i x -> let x = Char.code x in if x != 0 then begin
      let i = i lsl 3 in
      for j = 0 to 7 do if x land (1 lsl j) > 0 then f (i + j) done
    end)
    v.bits
*)
(* slightly more efficient by precomputing NTZ *)
let ntz = Array.make 256 0
let () = for i = 0 to 7 do ntz.(1 lsl i) <- i done
let ntz8 x = Array.unsafe_get ntz x

let iteri_true f v =
  Bytes.iteri
    (fun i c ->
       let i_bpi = i lsl 3 in
       let rec visit x =
	 if x != 0 then begin
	   let b = x land (-x) in
	   f (i_bpi + ntz8 b);
	   visit (x - b)
	 end
       in
       visit (Char.code c))
    v.bits

(* Population count *)

let rec naive_pop x =
  assert (x < 0x100);
  if x = 0 then 0 else 1 + naive_pop (x - (x land -x))

let pop8 = Array.init 0x100 naive_pop
let pop8 n = Array.unsafe_get pop8 n

let pop v =
  let n = Bytes.length v.bits in
  let b = v.bits in
  let rec loop acc i =
    if i >= n then acc else loop (acc + pop8 (byte b i)) (i + 1) in
  loop 0 0

(* Bitwise operations. It is straigthforward, since bitwise operations
   can be realized by the corresponding bitwise operations over integers.
   However, one has to take care of normalizing the result of [bwnot]
   which introduces ones in highest significant positions. *)

let[@inline always] bw_and_in_place_internal ~dst ~n b1 b2 =
  for i = 0 to n - 1 do
    set_byte dst i ((byte b1 i) land (byte b2 i))
  done

let bw_and_in_place ~dst v1 v2 =
  let l = v1.length in
  if l <> v2.length || l <> dst.length then invalid_arg "Bitv.bw_and_in_place";
  let b1 = v1.bits
  and b2 = v2.bits in
  let n = Bytes.length b1 in
  bw_and_in_place_internal ~dst:dst.bits ~n b1 b2

let bw_and v1 v2 =
  let l = v1.length in
  if l <> v2.length then invalid_arg "Bitv.bw_and";
  let b1 = v1.bits
  and b2 = v2.bits in
  let n = Bytes.length b1 in
  let a = Bytes.make n (Char.chr 0) in
  bw_and_in_place_internal ~dst:a ~n b1 b2;
  { length = l; bits = a }

let[@inline always] bw_or_in_place_internal ~dst ~n b1 b2 =
  for i = 0 to n - 1 do
    set_byte dst i ((byte b1 i) lor (byte b2 i))
  done

let bw_or_in_place ~dst v1 v2 =
  let l = v1.length in
  if l <> v2.length || l <> dst.length then invalid_arg "Bitv.bw_or_in_place";
  let b1 = v1.bits
  and b2 = v2.bits in
  let n = Bytes.length b1 in
  bw_or_in_place_internal ~dst:dst.bits ~n b1 b2

let bw_or v1 v2 =
  let l = v1.length in
  if l <> v2.length then invalid_arg "Bitv.bw_or";
  let b1 = v1.bits
  and b2 = v2.bits in
  let n = Bytes.length b1 in
  let a = Bytes.make n (Char.chr 0) in
  bw_or_in_place_internal ~dst:a ~n b1 b2;
  { length = l; bits = a }

let[@inline always] bw_xor_in_place_internal ~dst ~n b1 b2 =
  for i = 0 to n - 1 do
    set_byte dst i ((byte b1 i) lxor (byte b2 i))
  done

let bw_xor_in_place ~dst v1 v2 =
  let l = v1.length in
  if l <> v2.length || l <> dst.length then invalid_arg "Bitv.bw_xor_in_place";
  let b1 = v1.bits
  and b2 = v2.bits in
  let n = Bytes.length b1 in
  bw_xor_in_place_internal ~dst:dst.bits ~n b1 b2

let bw_xor v1 v2 =
  let l = v1.length in
  if l <> v2.length then invalid_arg "Bitv.bw_xor";
  let b1 = v1.bits
  and b2 = v2.bits in
  let n = Bytes.length b1 in
  let a = Bytes.make n (Char.chr 0) in
  bw_xor_in_place_internal ~dst:a ~n b1 b2;
  { length = l; bits = a }

let[@inline always] bw_not_in_place_internal ~dst ~n b =
  let a = dst.bits in
  for i = 0 to n - 1 do
    set_byte a i (255 land (lnot (byte b i)))
  done;
  normalize dst

let bw_not_in_place ~dst v =
  let l = v.length in
  if l <> dst.length then invalid_arg "Bitv.bw_not_in_place";
  let b = v.bits in
  let n = Bytes.length b in
  bw_not_in_place_internal ~dst ~n b

let bw_not v =
  let b = v.bits in
  let n = Bytes.length b in
  let a = Bytes.make n (Char.chr 0) in
  let dst = { length = v.length; bits = a } in
  bw_not_in_place_internal ~dst ~n b;
  dst

(* Coercions to/from lists of integers *)

let of_list l =
  let n = 1 + List.fold_left max (-1) l in
  if n < 0 || n > max_length then invalid_arg "Bitv.of_list";
  let b = create n false in
  let add_element i =
    (* negative numbers are invalid *)
    if i < 0 then invalid_arg "Bitv.of_list";
    unsafe_set b i true
  in
  List.iter add_element l;
  b

let of_list_with_length l len =
  if len < 0 || len > max_length then invalid_arg "Bitv.of_list_with_length";
  let b = create len false in
  let add_element i =
    if i < 0 || i >= len then invalid_arg "Bitv.of_list_with_length";
    unsafe_set b i true
  in
  List.iter add_element l;
  b

let to_list b =
  let n = length b in
  let rec make i acc =
    if i < 0 then acc
    else make (pred i) (if unsafe_get b i then i :: acc else acc)
  in
  make (pred n) []

let[@inline] pos n = n lsr 3, n land 7

let unsafe_getb b n =
  let i = n lsr 3 in
  (byte b i) land (1 lsl (n land 7)) > 0

let unsafe_setb b n v =
  let i = n lsr 3 in
  let c = byte b i in
  let mask = 1 lsl (n land 7) in
  set_byte b i (if v then c lor mask else c land (lnot mask))

(* Copies v1[ofs1..ofs1+len[ into v2[ofs2..ofs2+len[ *)
let unsafe_blit b1 ofs1 b2 ofs2 len =
  if len > 0 then
    if ofs1 land 7 = 0 && ofs2 land 7 = 0 && len land 7 = 0 then
      Bytes.blit b1 (ofs1 lsr 3) b2 (ofs2 lsr 3) (len lsr 3)
    else
      for i = 0 to len - 1 do
        unsafe_setb b2 (ofs2 + i) (unsafe_getb b1 (ofs1 + i))
      done
      (* TODO: improve in other cases when bytes can be batch-copied *)

let blit v1 ofs1 v2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > v1.length - len
             || ofs2 < 0 || ofs2 > v2.length - len
  then invalid_arg "Bitv.blit";
  unsafe_blit v1.bits ofs1 v2.bits ofs2 len

let sub v ofs len =
  if ofs < 0 || len < 0 || ofs > v.length - len then invalid_arg "Bitv.sub";
  let r = create len false in
  unsafe_blit v.bits ofs r.bits 0 len;
  r

let append v1 v2 =
  let l1 = v1.length
  and l2 = v2.length in
  let r = create (l1 + l2) false in
  let b1 = v1.bits in
  let b = r.bits in
  Bytes.blit b1 0 b 0 (Bytes.length b1);
  unsafe_blit v2.bits 0 b l1 l2;
  r

let concat vl =
  let size = List.fold_left (fun sz v -> sz + v.length) 0 vl in
  let res = create size false in
  let b = res.bits in
  let pos = ref 0 in
  List.iter
    (fun v ->
       let n = v.length in
       unsafe_blit v.bits 0 b !pos n;
       pos := !pos + n)
    vl;
  res

(* Testing for all zeros and all ones. *)

let all_zeros v =
  let b = v.bits in
  let n = Bytes.length b in
  let rec test i = i == n || (byte b i == 0) && test (succ i) in
  test 0

let all_ones v =
  let b = v.bits in
  let n = Bytes.length b in
  let rec test i =
    if i == n - 1 then
      let m = v.length land 7 in
      byte b i == if m == 0 then 0xFF else low_mask.(m)
    else
      byte b i == 0xFF && test (succ i)
  in
  n = 0 || test 0


(* Shift operations. It is easy to reuse [unsafe_blit], although it is
   probably slightly less efficient than a ad-hoc piece of code. *)

let rec shiftl v d =
  if d == 0 then
    copy v
  else if d < 0 then
    shiftr v (-d)
  else begin
    let n = v.length in
    let r = create n false in
    if d < n then unsafe_blit v.bits 0 r.bits d (n - d);
    r
  end

and shiftr v d =
  if d == 0 then
    copy v
  else if d < 0 then
    shiftl v (-d)
  else begin
    let n = v.length in
    let r = create n false in
    if d < n then unsafe_blit v.bits d r.bits 0 (n - d);
    r
  end

(* Rotate operations. It is easy to reuse [unsafe_blit], although it is
   probably slightly less efficient than an ad-hoc piece of code. *)

let rec rotatel v d =
  if d < 0 then
    rotater v (-d)
  else
  let n = v.length in
  if d == 0 || n == 0 then
    copy v
  else begin
    let d = d mod n in
    let r = create n false in
    unsafe_blit v.bits 0 r.bits d (n - d); (* shiftl *)
    unsafe_blit v.bits (n - d) r.bits 0 d; (* wraparound ms to ls *)
    r
  end

and rotater v d =
  if d < 0 then
    rotatel v (-d)
  else
  let n = v.length in
  if d == 0 || n == 0 then
    copy v
  else begin
    let d = d mod n in
    let r = create n false in
    unsafe_blit v.bits d r.bits 0 (n - d); (* shiftr *)
    unsafe_blit v.bits 0 r.bits (n - d) d; (* wraparound ls to ms *)
    r
  end

(* Iteration on all bit vectors of length [n] using a Gray code. *)

let first_set v n =
  let rec lookup i =
    if i = n then raise Not_found ;
    if unsafe_get v i then i else lookup (i + 1)
  in
  lookup 0

let gray_iter f n =
  let bv = create n false in
  let rec iter () =
    f bv;
    unsafe_set bv 0 (not (unsafe_get bv 0));
    f bv;
    let pos = succ (first_set bv n) in
    if pos < n then begin
      unsafe_set bv pos (not (unsafe_get bv pos));
      iter ()
    end
  in
  if n > 0 then iter ()

module S(I : sig val least_first : bool end) = struct

  let to_string v =
    let n = v.length in
    let s = Bytes.make n '0' in
    for i = 0 to n - 1 do
      if unsafe_get v i then Bytes.set s (if I.least_first then i else n-1-i) '1'
    done;
    Bytes.unsafe_to_string s

  let print fmt v = Format.pp_print_string fmt (to_string v)

  let of_string s =
    let n = String.length s in
    let v = create n false in
    for i = 0 to n - 1 do
      let c = String.unsafe_get s i in
      if c = '1' then
	unsafe_set v (if I.least_first then i else n-1-i) true
      else
	if c <> '0' then invalid_arg "Bitv.of_string"
    done;
    v

end
module L = S(struct let least_first = true end)
module M = S(struct let least_first = false end)

let tanimoto v1 v2 =
  let l = v1.length in
  if l <> v2.length then invalid_arg "Bitv.tanimoto";
  let a = pop v1 in
  let b = pop v2 in
  let c = pop (bw_and v1 v2) in
  (float c) /. (float (a + b - c))

(* Input/output in a machine-independent format. *)

let bytes_of_int x =
  Bytes.init 8 (fun i -> Char.chr ((x lsr (8 * i)) land 0xFF))

let int_of_bytes b =
  assert (Bytes.length b = 8);
  let rec build x i =
    if i < 0 then x
    else build ((x lsl 8) lor Char.code (Bytes.get b i)) (pred i)
  in
  build 0 7

let nb_of_bytes len =
  len lsr 3 + if len land 7 = 0 then 0 else 1

let to_bin write v =
  bytes_of_int v.length |> Bytes.iter write;
  Bytes.iter write v.bits

let output_bin out_ch v =
  let write = output_char out_ch in
  to_bin write v

let to_bytes t =
  let buf = Buffer.create 0 in
  let write x = Buffer.add_char buf x in
  to_bin write t;
  Buffer.to_bytes buf

let of_bin read =
  let len = Bytes.init 8 (fun _ -> read ()) |> int_of_bytes in
  let v = create len false in
  let b = v.bits in
  for i = 0 to Bytes.length b - 1 do
    Bytes.unsafe_set b i (read ())
  done;
  v

let input_bin in_ch =
  let read () = input_char in_ch in
  of_bin read

let of_bytes b =
  let read =
    let p = ref 0 in
    fun () -> let ret = Bytes.get b !p in incr p; ret in
  of_bin read

(*s To/from integers. *)

let of_int_gen len getbyte getbit =
  let v = create len false in
  for i = 0 to (len lsr 3) - 1 do set_byte v.bits i (getbyte i) done;
  for i = len land (lnot 7) to len - 1 do unsafe_set v i (getbit i) done;
  v

let getbyte x i = (x lsr (8*i)) land 0xFF
let getbit x i = (x lsr i) land 1 > 0
let of_int_us x =
  of_int_gen (Sys.int_size - 1) (getbyte x) (getbit x)
let of_int_s x =
  of_int_gen Sys.int_size (getbyte x) (getbit x)

let to_int_gen zero shiftor v =
  let x = ref zero in
  Bytes.iteri (fun i c -> x := shiftor !x (Char.code c) (8*i)) v.bits;
  !x

let shiftor x b i = x lor (b lsl i)
let to_int_us v =
  (* if v.length < Sys.int_size - 1 then invalid_arg "Bitv.to_int_us"; *)
  to_int_gen 0 shiftor v
let to_int_s v =
  (* if v.length < Sys.int_size then invalid_arg "Bitv.to_int_s"; *)
  to_int_gen 0 shiftor v

let getbyte32 x i =
  Int32.to_int (Int32.logand (Int32.shift_right x (8*i)) 0xFFl)
let getbit32 x i =
  Int32.logand (Int32.shift_right x i) 1l > 0l
let of_int32_us x =
  of_int_gen 31 (getbyte32 x) (getbit32 x)
let of_int32_s x =
  of_int_gen 32 (getbyte32 x) (getbit32 x)

let shiftor32 x b i = Int32.logor x (Int32.shift_left (Int32.of_int b) i)
let to_int32_us v = to_int_gen 0l shiftor32 v
let to_int32_s v = to_int_gen 0l shiftor32 v

let getbyte64 x i =
  Int64.to_int (Int64.logand (Int64.shift_right x (8*i)) 0xFFL)
let getbit64 x i =
  Int64.logand (Int64.shift_right x i) 1L > 0L
let of_int64_us x =
  of_int_gen 63 (getbyte64 x) (getbit64 x)
let of_int64_s x =
  of_int_gen 64 (getbyte64 x) (getbit64 x)

let shiftor64 x b i = Int64.logor x (Int64.shift_left (Int64.of_int b) i)
let to_int64_us v = to_int_gen 0L shiftor64 v
let to_int64_s v = to_int_gen 0L shiftor64 v

(* [Nativeint] *)
let select_of f32 f64 = match Sys.word_size with
  | 32 -> (fun i -> f32 (Nativeint.to_int32 i))
  | 64 -> (fun i -> f64 (Int64.of_nativeint i))
  | _ -> assert false
let of_nativeint_s = select_of of_int32_s of_int64_s
let of_nativeint_us = select_of of_int32_us of_int64_us
let select_to f32 f64 = match Sys.word_size with
  | 32 -> (fun i -> Nativeint.of_int32 (f32 i))
  | 64 -> (fun i -> Int64.to_nativeint (f64 i))
  | _ -> assert false
let to_nativeint_s = select_to to_int32_s to_int64_s
let to_nativeint_us = select_to to_int32_us to_int64_us
