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
  (** the type of persistent bit vectors *)

  (** Array interface *)

  val max_length: int
  val length: t -> int
  val make: int -> bool -> t
  val init: int -> (int -> bool) -> t
  val get: t -> int -> bool
  val set: t -> int -> bool -> t
  val iteri: (int -> bool -> unit) -> t -> unit
  val foldi: (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a
  (* TODO: fill, blit, sub, append *)

  (** Bit vector interface *)

  val swap: t -> int -> t
  val bw_and: t -> t -> t
  val bw_or: t -> t -> t
  val bw_xor: t -> t -> t
  val bw_not: t -> t
  val pop: t -> int
  val ntz: t -> int
  val nlz: t -> int
  val print: Format.formatter -> t -> unit
   (** prints a bit vectors using 0s and 1s, from most significant bits
       to least significant bits *)

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int

  val unsafe_get: t -> int -> bool
  val unsafe_set: t -> int -> bool -> t

  (** Set interface

  The following functions interpret a bit vector as the characteristic
  predicate of a set, i.e. the elements of the set are the indices at
  which the bit vector is true.

  The iteration functions below are only iterating over the elements
  of the set, i.e. over the 1 bits of the bit vector, and not over all
  the bits. (To iterate over the bits, use [iteri] and [foldi] above.)  *)
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
  val iteri_true: (elt -> unit) -> t -> unit
  val foldi_true: (elt -> 'a -> 'a) -> t -> 'a -> 'a
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
  val to_seq_from: elt -> t -> elt Seq.t
  val to_seq: t -> elt Seq.t
  val to_rev_seq: t -> elt Seq.t
  val add_seq: elt Seq.t -> t -> t
  val of_seq: elt Seq.t -> t
  val print_set: Format.formatter -> t -> unit
    (** prints a bit vector as a set, using notation [{x1,x2,...,xn}]. *)
  (* TODO iter_subsets *)
end

module Native : S
  (** Bit vectors of size [Sys.int_size], implemented using a machine integer.
      Note: The size parameter of [empty] and [full] is ignored,
      and operations [sub] and [append] are not supported. *)

module Small(X: sig val size: int end) : S
  (** Bit vectors of size at most [Sys.int_size], implemented using a
      machine integer. *)

module Large : S
  (** Bit vectors of arbitrary size, up to [2**31 - 1]. *)

val fixed_size: int -> (module S)
  (** Bit vectors of fixed size. The relevant implementation is selected:
      either a single machine integer when the size is small enough, or
      large bit vectors otherwise.

      Operations [sub] and [append] are not supported. *)
