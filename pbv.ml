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
  val singleton: int -> int -> t
  val is_empty: t -> bool
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

  let empty _n =
    0

  let full _n =
    -1

  let singleton _len i =
    1 lsl i

  let is_empty v =
    v == 0

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

  let pop _v = assert false (*TODO*)
  let ntz _v = assert false (*TODO*)
  let nlz _v = assert false (*TODO*)

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

