(*
 * Bit vectors.
 * Copyright (C) 1999 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* $Id: bitv.ml,v 1.3 1999/03/18 17:34:05 filliatr Exp $ *)

(* Bit vectors. The interface and part of the code are borrowed from the 
 * Array module of the ocaml standard library (but things are simplified
 * here since we can always initialize a bit vector). *)

let bpi = Sys.word_size - 2  (* bits per int (30 or 62) *)

let bit_j = Array.init bpi (fun j -> 1 lsl j)
let bit_not_j = Array.init bpi (fun j -> max_int - bit_j.(j))

let max_length = Sys.max_array_length * bpi

(* We represent a bit vector by a vector of integers, and we keep the
 * information of the size of the bit vector since it can not be found out
 * with the size of the array. 
 * true is represented by 1 and false by zero, as usual. *)

type t =
    { length : int;      (* length of the vector *)
      bits   : int array (* bits array *) }

let create n b =
  let s = let s = n / bpi in if n mod bpi = 0 then s else succ s in
  let initv = if b then max_int else 0 in
  { length = n;
    bits = Array.create s initv }

let length v = v.length

let pos n = (n / bpi, n mod bpi)

(* Unsafe operations *)

let unsafe_set v n b =
  let (i,j) = pos n in
  if b then
    Array.unsafe_set v.bits i 
      ((Array.unsafe_get v.bits i) lor (Array.unsafe_get bit_j j))
  else 
    Array.unsafe_set v.bits i 
      ((Array.unsafe_get v.bits i) land (Array.unsafe_get bit_not_j j))

let unsafe_get v n =
  let (i,j) = pos n in 
  ((Array.unsafe_get v.bits i) land (Array.unsafe_get bit_j j)) > 0

(* Safe operations *)

let set v n b =
  if n < 0 or n >= v.length then invalid_arg "Bitv.set";
  let (i,j) = pos n in
  if b then
    Array.unsafe_set v.bits i
      ((Array.unsafe_get v.bits i) lor (Array.unsafe_get bit_j j))
  else
    Array.unsafe_set v.bits i
      ((Array.unsafe_get v.bits i) land (Array.unsafe_get bit_not_j j))

let get v n =
  if n < 0 or n >= v.length then invalid_arg "Bitv.set";
  let (i,j) = pos n in 
  ((Array.unsafe_get v.bits i) land (Array.unsafe_get bit_j j)) > 0

let init n f =
  let v = create n false in
  for i = 0 to pred n do
    unsafe_set v i (f i)
  done;
  v

let copy v =
  { length = v.length;
    bits = Array.copy v.bits }

let append v1 v2 =
  let l1 = v1.length 
  and l2 = v2.length in
  let r = create (l1 + l2) false in
  for i = 0 to l1 - 1 do unsafe_set r i (unsafe_get v1 i) done;  
  for i = 0 to l2 - 1 do unsafe_set r (i + l1) (unsafe_get v2 i) done;  
  r

let concat vl =
  let size = List.fold_left (fun sz v -> sz + v.length) 0 vl in
  let res = create size false in
  let pos = ref 0 in
  List.iter
    (fun v ->
       for i = 0 to v.length - 1 do
         unsafe_set res !pos (unsafe_get v i);
         incr pos
       done)
    vl;
  res

let sub v ofs len =
  if ofs < 0 or len < 0 or ofs + len > v.length then invalid_arg "Bitv.sub";
  let r = create len false in
  for i = 0 to len - 1 do unsafe_set r i (unsafe_get v (ofs + i)) done;
  r

let fill v ofs len b =
  if ofs < 0 or len < 0 or ofs + len > v.length then invalid_arg "Bitv.fill";
  for i = ofs to ofs + len - 1 do unsafe_set v i b done

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 or ofs1 < 0 or ofs1 + len > length a1
             or ofs2 < 0 or ofs2 + len > length a2
  then invalid_arg "Bitv.blit";
  if ofs1 < ofs2 then
    (* Top-down copy *)
    for i = len - 1 downto 0 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done
  else
    (* Bottom-up copy *)
    for i = 0 to len - 1 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done

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
