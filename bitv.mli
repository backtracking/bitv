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

(* $Id: bitv.mli,v 1.1 1999/02/14 17:45:01 filliatr Exp $ *)


type t
    (* The type of bit vectors. *)

val create : int -> bool -> t
    (* [Bitv.create n b] creates a new bit vector of length [n],
     * initialized with [b]. *)

val length : t -> int
    (* Return the length (number of elements) of the given vector. *)

val set : t -> int -> bool -> unit
    (* [Bitv.set v n b] sets the [n]th bit of [v] to the value [b]. *)
    
val get : t -> int -> bool
    (* [Bitv.get v n] returns the [n]th bit of [v]. *)

val init : int -> (int -> bool) -> t
    (* [Array.init n f] returns a fresh vector of length [n],
       with bit number [i] initialized to the result of [f i]. *)

val copy: t -> t
        (* [Bitv.copy v] returns a copy of [v], that is, a fresh vector
           containing the same elements as [v]. *)

val append: t -> t -> t
        (* [Bitv.append v1 v2] returns a fresh vector containing the
           concatenation of the vectors [v1] and [v2]. *)

val concat: t list -> t
        (* Same as [Bitv.append], but catenates a list of arrays. *)

val sub: t -> int -> int -> t
        (* [Bitv.sub v start len] returns a fresh vector of length [len],
           containing the bits number [start] to [start + len - 1]
           of vector [v].
           Raise [Invalid_argument "Bitv.sub"] if [start] and [len] do not
           designate a valid subvector of [v]; that is, if
           [start < 0], or [len < 0], or [start + len > Bitv.length a]. *)

val fill: t -> int -> int -> bool -> unit
        (* [Bitv.fill v ofs len b] modifies the vector [v] in place,
           storing [b] in elements number [ofs] to [ofs + len - 1].
           Raise [Invalid_argument "Bitv.fill"] if [ofs] and [len] do not
           designate a valid subvector of [v]. *)

val blit: t -> int -> t -> int -> int -> unit
        (* [Bitv.blit v1 o1 v2 o2 len] copies [len] elements
           from vector [v1], starting at element number [o1], to vector [v2],
           starting at element number [o2]. It works correctly even if
           [v1] and [v2] are the same array, and the source and
           destination chunks overlap.
           Raise [Invalid_argument "Bitv.blit"] if [o1] and [len] do not
           designate a valid subvector of [v1], or if [o2] and [len] do not
           designate a valid subvector of [v2]. *)

val iter: (bool -> unit) -> t -> unit
        (* [Bitv.iter f v] applies function [f] in turn to all
           the elements of [v]. *)

val map: (bool -> bool) -> t -> t
        (* [Bitv.map f v] applies function [f] to all the elements of [v],
           and builds a vector with the results returned by [f]. *)

val iteri: (int -> bool -> unit) -> t -> unit
val mapi: (int -> bool -> bool) -> t -> t
        (* Same as [Bitv.iter] and [Bitv.map] respectively, but the
           function is applied to the index of the element as first argument,
           and the element itself as second argument. *)

val fold_left: ('a -> bool -> 'a) -> 'a -> t -> 'a
        (* [Bitv.fold_left f x v] computes
           [f (... (f (f x (get v 0)) (get v 1)) ...) (get v (n-1))],
           where [n] is the length of the vector [v]. *)

val fold_right: (bool -> 'a -> 'a) -> t -> 'a -> 'a
        (* [Bitv.fold_right f a x] computes
           [f (get v 0) (f (get v 1) ( ... (f (get v (n-1)) x) ...))],
           where [n] is the length of the vector [v]. *)


(* -- *)

val unsafe_set : t -> int -> bool -> unit
val unsafe_get : t -> int -> bool
    (* Only if you know what you are doing... *)
