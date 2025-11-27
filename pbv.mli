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

(** This module implements persistent bit vectors.

    For imperative bit vectors, see module [Bitv].

    In the following, [false] stands for bit 0 and [true] for bit 1.
*)

module type S = sig
  type t

  (** Array interface *)

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

  (** Set interface *)

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

module Native : S

val fixed_size: int -> (module S)
