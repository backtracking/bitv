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

(*i $Id: bitv.mli,v 1.10 2002/11/22 08:25:47 filliatr Exp $ i*)

(*s {\bf Module Bitv}.
    This module implements bit vectors, as an abstract datatype [t]. 
    Since bit vectors are particular cases of arrays, this module provides
    the same operations as the module [Array] (Sections~\ref{barray} 
    up to \ref{earray}). It also provides bitwise operations 
    (Section~\ref{bitwise}). In the following, [false] stands for the bit 0 
    and [true] for the bit 1. *)

type t

(*s {\bf Creation, access and assignment.} \label{barray}
    [(Bitv.create n b)] creates a new bit vector of length [n],
    initialized with [b].
    [(Bitv.init n f)] returns a fresh vector of length [n],
    with bit number [i] initialized to the result of [(f i)]. 
    [(Bitv.set v n b)] sets the [n]th bit of [v] to the value [b].
    [(Bitv.get v n)] returns the [n]th bit of [v]. 
    [Bitv.length] returns the length (number of elements) of the given 
    vector. *)

val create : int -> bool -> t

val init : int -> (int -> bool) -> t

val set : t -> int -> bool -> unit
    
val get : t -> int -> bool

val length : t -> int

(*s [max_length] is the maximum length of a bit vector (System dependent). *)

val max_length : int

(*s {\bf Copies and concatenations.}
   [(Bitv.copy v)] returns a copy of [v],
   that is, a fresh vector containing the same elements as
   [v]. [(Bitv.append v1 v2)] returns a fresh vector containing the
   concatenation of the vectors [v1] and [v2]. [Bitv.concat] is
   similar to [Bitv.append], but catenates a list of vectors. *)

val copy : t -> t

val append : t -> t -> t

val concat : t list -> t

(*s {\bf Sub-vectors and filling.} 
    [(Bitv.sub v start len)] returns a fresh
    vector of length [len], containing the bits number [start] to
    [start + len - 1] of vector [v].  Raise [Invalid_argument
    "Bitv.sub"] if [start] and [len] do not designate a valid
    subvector of [v]; that is, if [start < 0], or [len < 0], or [start
    + len > Bitv.length a].

    [(Bitv.fill v ofs len b)] modifies the vector [v] in place,
    storing [b] in elements number [ofs] to [ofs + len - 1].  Raise
    [Invalid_argument "Bitv.fill"] if [ofs] and [len] do not designate
    a valid subvector of [v].

    [(Bitv.blit v1 o1 v2 o2 len)] copies [len] elements from vector
    [v1], starting at element number [o1], to vector [v2], starting at
    element number [o2]. It {\em does not work} correctly if [v1] and [v2] are
    the same vector with the source and destination chunks overlapping.
    Raise [Invalid_argument "Bitv.blit"] if [o1] and [len] do not
    designate a valid subvector of [v1], or if [o2] and [len] do not
    designate a valid subvector of [v2]. *)

val sub : t -> int -> int -> t

val fill : t -> int -> int -> bool -> unit

val blit : t -> int -> t -> int -> int -> unit

(*s {\bf Iterators.} \label{earray}
    [(Bitv.iter f v)] applies function [f] in turn to all
    the elements of [v]. Given a function [f], [(Bitv.map f v)] applies
    [f] to all
    the elements of [v], and builds a vector with the results returned
    by [f]. [Bitv.iteri] and [Bitv.mapi] are similar to [Bitv.iter]
    and [Bitv.map] respectively, but the function is applied to the
    index of the element as first argument, and the element itself as
    second argument.

    [(Bitv.fold_left f x v)] computes [f (... (f (f x (get v 0)) (get
    v 1)) ...) (get v (n-1))], where [n] is the length of the vector
    [v]. 

    [(Bitv.fold_right f a x)] computes [f (get v 0) (f (get v 1)
    ( ... (f (get v (n-1)) x) ...))], where [n] is the length of the
    vector [v]. *)

val iter : (bool -> unit) -> t -> unit
val map : (bool -> bool) -> t -> t

val iteri : (int -> bool -> unit) -> t -> unit
val mapi : (int -> bool -> bool) -> t -> t

val fold_left : ('a -> bool -> 'a) -> 'a -> t -> 'a
val fold_right : (bool -> 'a -> 'a) -> t -> 'a -> 'a

(*s [gray_iter f n] iterates function [f] on all bit vectors
    of length [n], once each, using a Gray code. The order in which
    bit vectors are processed is unspecified. *)

val gray_iter : (t -> unit) -> int -> unit

(*s {\bf Bitwise operations.} \label{bitwise} [bwand], [bwor] and
    [bwxor] implement logical and, or and exclusive or.  They return
    fresh vectors and raise [Invalid_argument "Bitv.xxx"] if the two
    vectors do not have the same length (where \texttt{xxx} is the
    name of the function).  [bwnot] implements the logical negation. 
    It returns a fresh vector.
    [shiftl] and [shiftr] implement shifts. They return fresh vectors.
    [shiftl] moves bits from least to most significant, and [shiftr]
    from most to least significant (think [lsl] and [lsr]).
    [all_zeros] and [all_ones] respectively test for a vector only
    containing zeros and only containing ones. *)

val bw_and : t -> t -> t
val bw_or  : t -> t -> t
val bw_xor : t -> t -> t
val bw_not : t -> t

val shiftl : t -> int -> t
val shiftr : t -> int -> t

val all_zeros : t -> bool
val all_ones  : t -> bool

(*s {\bf Conversions to and from strings.} 
    Least significant bit comes first. *)

val to_string : t -> string
val from_string : string -> t
val print : Format.formatter -> t -> unit

(*s {\bf Conversions to and from lists of integers.} *)

val to_list : t -> int list
val from_list : int list -> t
val from_list_with_length : int list -> int -> t

(*s Only if you know what you are doing... *)

val unsafe_set : t -> int -> bool -> unit
val unsafe_get : t -> int -> bool
