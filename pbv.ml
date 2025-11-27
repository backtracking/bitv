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

module type S = sig
  type t
  val length: t -> int
  val make: int -> bool -> t
  val get: t -> int -> bool
  val set: t -> int -> bool -> t
  val swap: t -> int -> t

  val bw_and: t -> t -> t
  val bw_or: t -> t -> t
  val bw_xor: t -> t -> t
  val bw_not: t -> t
  val pop: t -> int
  val ntz: t -> int
  val nlz: t -> int

  val empty: int -> t
  val full: int -> t
  val cardinal: t -> int
  val singleton: int -> int -> t
  val is_empty: t -> bool
  val min_elt: t -> int
  val max_elt: t -> int
  val add: t -> int -> t
  val remove: t -> int -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
end

module Native = struct

  type t = int

  let length _v =
    Sys.int_size

  let make _n b =
    if b then -1 else 0

  let get v i =
    (v lsr i) land 1 <> 0

  let set v i b =
    if b then v lor (1 lsl i) else v land (lnot (1 lsl i))

  let add v i =
    set v i true

  let remove v i =
    set v i false

  let swap v i =
    v lxor (1 lsl i)

  let bw_or  = (lor)
  let bw_and = (land)
  let bw_xor = (lxor)
  let bw_not = (lnot)

  let rec naive_pop x =
    assert (x < 0x10000);
    if x = 0 then 0 else 1 + naive_pop (x - (x land -x))
  let pop16 = Array.init 0x10000 naive_pop
  let pop16 x = Array.unsafe_get pop16 x
  let pop32 x = pop16 (x land 0xffff) + pop16 ((x lsr 16) land 0xffff)
  let pop64 x = pop16 (x land 0xffff) + pop16 ((x lsr 16) land 0xffff)
              + pop16 ((x lsr 32) land 0xffff) + pop16 ((x lsr 48) land 0xffff)
  let pop =
    match Sys.word_size with 32 -> pop32 | 64 -> pop64 | _ -> assert false

  (* inverse of `1 lsl i` i.e. tib i = log_2(i) *)
  let log2 = Array.make 255 0
  let () = for i = 0 to 7 do log2.(1 lsl i) <- i done

  (* assumption: x is a power of 2 *)
  let tib32 x =
    if x land 0xFFFF == 0 then
      let x = x lsr 16 in
      if x land 0xFF == 0 then 24 + log2.(x lsr 8) else 16 + log2.(x)
    else
      if x land 0xFF == 0 then 8 + log2.(x lsr 8) else log2.(x)

  let ffffffff = (0xffff lsl 16) lor 0xffff
  let tib64 x =
    if x land ffffffff == 0 then 32 + tib32 (x lsr 32) else tib32 x
  let tib =
    match Sys.word_size with 32 -> tib32 | 64 -> tib64 | _ -> assert false

  let ntz v =
    if v == 0 then invalid_arg "ntz";
    tib (v land (-v))

  let nlz v =
    if v == 0 then invalid_arg "nlz";
    let rec loop i =
      if v land i != 0 then Sys.int_size - 1 - tib i else loop (i lsr 1) in
    loop min_int

  let empty _n = 0
  let full _n = -1
  let cardinal = pop
  let singleton _len i = 1 lsl i
  let is_empty v = v == 0
  let min_elt v =
    if is_empty v then invalid_arg "min_elt";
    ntz v
  let max_elt v =
    if is_empty v then invalid_arg "min_elt";
    Sys.int_size - 1 - nlz v
  let union = bw_or
  let inter = bw_and
  let diff v1 v2 = v1 land (lnot v2)
  let subset v1 v2 = v1 land (lnot v2) == 0

end

let fixed_size n : (module S) =
  if n = Sys.int_size then
    (module Native)
  else
    assert false (*TODO*)

