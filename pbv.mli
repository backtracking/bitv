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

  val max_length: int
  val length: t -> int
  val make: int -> bool -> t
  val get: t -> int -> bool
  val set: t -> int -> bool -> t
  (* TODO: iter, print, fill, blit, sub, append *)

  (** Bit vector interface *)

  val swap: t -> int -> t
  val bw_and: t -> t -> t
  val bw_or: t -> t -> t
  val bw_xor: t -> t -> t
  val bw_not: t -> t
  val pop: t -> int
  val ntz: t -> int
  val nlz: t -> int

  (** Set interface *)

  type size = int
  type elt = int
  val empty: size -> t
  val is_empty: t -> bool
  val full: size -> t
  val mem: elt -> t -> bool
  val cardinal: t -> int (* same as pop *)
  val singleton: size -> elt -> t
  val min_elt: t -> elt
  val min_elt_opt: t -> elt option
  val max_elt: t -> elt
  val max_elt_opt: t -> elt option
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
  val disjoint: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val map: (elt -> elt) -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val filter_map: (elt -> elt option) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val elements: t -> elt list
  val choose: t -> elt
  val choose_opt: t -> elt option
  val split: elt -> t -> t * bool * t
  val find: elt -> t -> elt
  val find_opt: elt -> t -> elt option
  val find_first: (elt -> bool) -> t -> elt
  val find_first_opt: (elt -> bool) -> t -> elt option
  val find_last: (elt -> bool) -> t -> elt
  val find_last_opt: (elt -> bool) -> t -> elt option
  val of_list: elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
  val print_set: Format.formatter -> t -> unit

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end

module Native : S
  (** Bit-vectors of size [Sys.int_size].
      Note: The size parameter of [empty] and [full] is ignored. *)

val fixed_size: int -> (module S)
