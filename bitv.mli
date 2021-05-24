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

(** This module implements bit vectors, as an abstract datatype [t].
    Since bit vectors are particular cases of arrays, this module provides
    the same operations as module [Array]. It also provides bitwise operations
    and conversions to/from integer types.

    In the following, [false] stands for bit 0 and [true] for bit 1. *)

type t
(** the type of bit vectors *)

(** {2 Creation, access and assignment.} *)

val create : int -> bool -> t
(** [(Bitv.create n b)] creates a new bit vector of length [n],
   initialized with [b]. *)

val init : int -> (int -> bool) -> t
(** [(Bitv.init n f)] returns a fresh vector of length [n],
    with bit number [i] initialized to the result of [(f i)]. *)

val set : t -> int -> bool -> unit
(** [(Bitv.set v n b)] sets the [n]th bit of [v] to the value [b]. *)

val get : t -> int -> bool
(** [(Bitv.get v n)] returns the [n]th bit of [v]. *)

val length : t -> int
(** [Bitv.length] returns the length (number of elements) of the given
    vector. *)

val equal : t -> t -> bool
(** Returns [true] if two bit vectors are of the same length and
    with the same bits. *)

val max_length : int
(** @deprecated Use [exceeds_max_length] instead.
    On a 32-bit platform (e.g. Javascript) the computation of [max_length]
    may overflow and return a negative value. *)

val exceeds_max_length : int -> bool
(** Returns true if the argument exceeds the maximum length of a bit vector
    (System dependent). *)

(** {2 Copies and concatenations.} *)

val copy : t -> t
(** [(Bitv.copy v)] returns a copy of [v],
   that is, a fresh vector containing the same elements as [v]. *)

val append : t -> t -> t
(** [(Bitv.append v1 v2)] returns a fresh vector containing the
   concatenation of the vectors [v1] and [v2]. *)

val concat : t list -> t
(** [Bitv.concat] is similar to [Bitv.append], but catenates a list of
   vectors. *)

(** {2 Sub-vectors and filling.} *)

val sub : t -> int -> int -> t
(** [(Bitv.sub v start len)] returns a fresh
    vector of length [len], containing the bits number [start] to
    [start + len - 1] of vector [v].  Raise [Invalid_argument
    "Bitv.sub"] if [start] and [len] do not designate a valid
    subvector of [v]; that is, if [start < 0], or [len < 0], or [start
    + len > Bitv.length a]. *)

val fill : t -> int -> int -> bool -> unit
(** [(Bitv.fill v ofs len b)] modifies the vector [v] in place,
    storing [b] in elements number [ofs] to [ofs + len - 1].  Raise
    [Invalid_argument "Bitv.fill"] if [ofs] and [len] do not designate
    a valid subvector of [v]. *)

val blit : t -> int -> t -> int -> int -> unit
(** [(Bitv.blit v1 o1 v2 o2 len)] copies [len] elements from vector
    [v1], starting at element number [o1], to vector [v2], starting at
    element number [o2]. It does not work correctly if [v1] and [v2] are
    the same vector with the source and destination chunks overlapping.
    Raise [Invalid_argument "Bitv.blit"] if [o1] and [len] do not
    designate a valid subvector of [v1], or if [o2] and [len] do not
    designate a valid subvector of [v2]. *)

(** {2 Iterators} *)

val iter : (bool -> unit) -> t -> unit
(** [(Bitv.iter f v)] applies function [f] in turn to all
    the elements of [v]. *)

val map : (bool -> bool) -> t -> t
(** Given a function [f], [(Bitv.map f v)] applies [f] to all
    the elements of [v], and builds a vector with the results returned
    by [f]. *)

val iteri : (int -> bool -> unit) -> t -> unit
val mapi : (int -> bool -> bool) -> t -> t
(** [Bitv.iteri] and [Bitv.mapi] are similar to [Bitv.iter]
    and [Bitv.map] respectively, but the function is applied to the
    index of the element as first argument, and the element itself as
    second argument. *)

val fold_left : ('a -> bool -> 'a) -> 'a -> t -> 'a
(** [(Bitv.fold_left f x v)] computes [f (... (f (f x (get v 0)) (get
    v 1)) ...) (get v (n-1))], where [n] is the length of the vector
    [v]. *)

val fold_right : (bool -> 'a -> 'a) -> t -> 'a -> 'a
(** [(Bitv.fold_right f a x)] computes [f (get v 0) (f (get v 1)
    ( ... (f (get v (n-1)) x) ...))], where [n] is the length of the
    vector [v]. *)

val foldi_left : ('a -> int -> bool -> 'a) -> 'a -> t -> 'a
val foldi_right : (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a

(** {2 Pop count and other iterations} *)

val pop: t -> int
(** Population count, i.e., number of 1 bits *)

val iteri_true : (int -> unit) -> t -> unit
(** [iteri_true f v] applies function [f] in turn to all indexes of
    the elements of [v] which are set (i.e. [true]); indexes are
    visited from least significant to most significant. *)

val gray_iter : (t -> unit) -> int -> unit
(** [gray_iter f n] iterates function [f] on all bit vectors
  of length [n], once each, using a Gray code. The order in which
  bit vectors are processed is unspecified. *)

(** {2 Bitwise operations.}

    All the bitwise operations return fresh vectors. *)

val bw_and : t -> t -> t
(** bitwise AND;
    raises [Invalid_argument] if the two vectors do not have the same length *)

val bw_or  : t -> t -> t
(** bitwise OR;
    raises [Invalid_argument] if the two vectors do not have the same length *)

val bw_xor : t -> t -> t
(** bitwise XOR;
    raises [Invalid_argument] if the two vectors do not have the same length *)

val bw_not : t -> t
(** bitwise NOT *)

val shiftl : t -> int -> t
(** moves bits from least to most significant; introduces zeros *)

val shiftr : t -> int -> t
(** moves bits from most to least significant; introduces zeros *)

val rotatel : t -> int -> t
(** moves bits from least to most significant with wraparound *)

val rotater : t -> int -> t
(** moves bits from most to least significant with wraparound *)

(** {2 Test functions} *)

val all_zeros : t -> bool
(** returns [true] if and only if the vector only contains zeros *)

val all_ones  : t -> bool
(** returns [true] if and only if the vector only contains ones *)

(** {2 Conversions to and from strings} *)

(** With least significant bits first. *)
module L : sig
  val to_string : t -> string
  val of_string : string -> t
  val print : Format.formatter -> t -> unit
end

(** With most significant bits first. *)
module M : sig
  val to_string : t -> string
  val of_string : string -> t
  val print : Format.formatter -> t -> unit
end

(** {2 Input/output in a machine-independent format}

    The following functions export/import a bit vector to/from a channel or
    bytes, in a way that is compact, independent of the machine architecture,
    and independent of the OCaml version.
    For a bit vector of length [n], the number of bytes of this external
    representation is 8+ceil(n/8). *)

val length_bin: t -> int

val output_bin: out_channel -> t -> unit
val input_bin: in_channel -> t

val to_bytes: t -> bytes
val of_bytes: bytes -> t

val iter_bin: t -> (char -> unit) -> unit
(** [iter_bin v f] will call [f byte] for each [byte] it intends to write. *)

val from_stream_bin: (unit -> char) -> t
(** [from_stream_bin f] will call [f ()] for read the next byte to decode. *)

(** {2 Conversions to and from lists of integers}

    The list gives the indices of bits which are set (ie [true]). *)

val to_list : t -> int list
val of_list : int list -> t
val of_list_with_length : int list -> int -> t

(** {2 Interpretation of bit vectors as integers}

    Least significant bit
    comes first (ie is at index 0 in the bit vector).
    [to_xxx] functions truncate when the bit vector is too wide,
    and raise [Invalid_argument] when it is too short.
    Suffix [_s] means that sign bit is kept,
    and [_us] that it is discarded. *)

(** {3 type [int] (length 31/63 with sign, 30/62 without)} *)

val of_int_s : int -> t
val to_int_s : t -> int
val of_int_us : int -> t
val to_int_us : t -> int

(** {3 type [Int32.t] (length 32 with sign, 31 without)} *)

val of_int32_s : Int32.t -> t
val to_int32_s : t -> Int32.t
val of_int32_us : Int32.t -> t
val to_int32_us : t -> Int32.t

(** {3 type [Int64.t] (length 64 with sign, 63 without)} *)

val of_int64_s : Int64.t -> t
val to_int64_s : t -> Int64.t
val of_int64_us : Int64.t -> t
val to_int64_us : t -> Int64.t

(** {3 type [Nativeint.t] (length 32/64 with sign, 31/63 without)} *)

val of_nativeint_s : Nativeint.t -> t
val to_nativeint_s : t -> Nativeint.t
val of_nativeint_us : Nativeint.t -> t
val to_nativeint_us : t -> Nativeint.t

(** {2 Only if you know what you are doing...} *)

val unsafe_set : t -> int -> bool -> unit
val unsafe_get : t -> int -> bool
