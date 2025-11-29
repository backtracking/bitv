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
  val max_length: int
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

  type size = int
  type elt = int
  val empty: size -> t
  val is_empty: t -> bool
  val full: size -> t
  val mem: elt -> t -> bool
  val cardinal: t -> int
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

module Native = struct

  type t = int (* including the sign bit *)

  let compare = Int.compare
  let equal = (==)
  let hash v = v

  type size = int
  type elt = int

  let max_length =
    Sys.int_size

  let length _v =
    Sys.int_size

  let make _n b =
    if b then -1 else 0

  let unsafe_get v i =
    (v lsr i) land 1 <> 0

  let check_index s v i =
    if i < 0 || i >= length v then invalid_arg s

  let get v i =
    check_index "get" v i;
    unsafe_get v i

  let unsafe_set v i b =
    if b then v lor (1 lsl i) else v land (lnot (1 lsl i))

  let set v i b =
    check_index "set" v i;
    unsafe_set v i b

  let add i v =
    check_index "add" v i;
    set v i true

  let remove i v =
    check_index "remove" v i;
    set v i false

  let swap v i =
    check_index "swap" v i;
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
  let mem i v = get v i
  let cardinal = pop
  let singleton len i =
    if i < 0 || i >= len then invalid_arg "singleton";
    1 lsl i
  let is_empty v = v == 0
  let min_elt v =
    if is_empty v then invalid_arg "min_elt";
    ntz v
  let min_elt_opt v =
    if is_empty v then None else Some (ntz v)
  let max_elt v =
    if is_empty v then invalid_arg "min_elt";
    Sys.int_size - 1 - nlz v
  let max_elt_opt v =
    if is_empty v then None else Some (Sys.int_size - 1 - nlz v)

  let find i s = if mem i s then i else raise Not_found
  let find_opt i s = if mem i s then Some i else None

  let choose = min_elt
  let choose_opt = min_elt_opt

  let union = bw_or
  let inter = bw_and
  let diff v1 v2 = v1 land (lnot v2)
  let subset v1 v2 = v1 land (lnot v2) == 0
  let disjoint v1 v2 = v1 land v2 == 0

  let find_first p v =
    let rec loop v =
      if v = 0 then raise Not_found;
      let b = v land (-v) in
      let x = tib b in
      if p x then x else loop (v - b) in
    loop v

  let find_first_opt p v =
    try Some (find_first p v) with Not_found -> None

  let find_last p v =
    if v == 0 then raise Not_found;
    let rec loop b =
      let x = tib b in
      if v land b != 0 && p x then x
      else if b = 1 then raise Not_found else loop (b lsr 1) in
    loop min_int

  let find_last_opt p v =
    try Some (find_last p v) with Not_found -> None

  let rec elements v =
    if v == 0 then [] else let i = v land (-v) in tib i :: elements (v - i)

  let rec iter f v =
    if v != 0 then let i = v land (-v) in f (tib i); iter f (v - i)

  let rec fold f v acc =
    if v == 0 then acc else let i = v land (-v) in fold f (v - i) (f (tib i) acc)

  let rec for_all p v =
    v == 0 || let i = v land (-v) in p (tib i) && for_all p (v - i)

  let rec exists p v =
    v != 0 && let i = v land (-v) in p (tib i) || exists p (v - i)

  let rec filter p v =
    if v == 0 then
      0
    else
      let i = v land (-v) in
      let v = filter p (v - i) in
      if p (tib i) then v + i else v

  let rec filter_map f v =
    if v == 0 then
      0
    else
      let i = v land (-v) in
      let v = filter_map f (v - i) in
      match f (tib i) with
      | None -> v
      | Some x -> add x v

  let rec partition p v =
     if v == 0 then
      0, 0
    else
      let i = v land (-v) in
      let vt,sf = partition p (v - i) in
      if p (tib i) then vt + i, sf else vt, sf + i

  let split i v =
    let bi = 1 lsl i in
    v land (bi - 1), v land bi != 0, v land (-1 lsl (i+1))

  let print_set fmt v =
    let rec pr = function
      | [] -> ()
      | x :: l ->
          Format.fprintf fmt "%d" x; if l <> [] then Format.fprintf fmt ",@,";
          pr l
    in
    Format.fprintf fmt "{";
    pr (elements v);
    Format.fprintf fmt "}"

  let map f v =
    fold (fun x v -> add (f x) v) v 0(*(empty (length v))*)

  let of_list =
    List.fold_left (fun s x -> add x s) 0(*(empty (List.length l))*)

  let of_seq =
    Seq.fold_left (fun v x -> add x v) 0(*(empty (Seq.length s))*)

  let rec to_seq_from x v =
    if x > max_elt v then Seq.empty
    else if mem x v then fun () -> Seq.Cons (x, to_seq_from (x + 1) v)
    else to_seq_from (x + 1) v

  let to_seq v =
    if is_empty v then Seq.empty else to_seq_from (min_elt v) v

  let rec to_rev_seq_from x v =
    if x < min_elt v then Seq.empty
    else if mem x v then fun () -> Seq.Cons (x, to_rev_seq_from (x - 1) v)
    else to_rev_seq_from (x - 1) v

  let to_rev_seq v =
    if is_empty v then Seq.empty else to_rev_seq_from (max_elt v) v

  let rec add_seq veq v = match veq () with
    | Seq.Nil -> v
    | Seq.Cons (x, veq) -> add_seq veq (add x v)

end

let fixed_size n : (module S) =
  if n = Sys.int_size then
    (module Native)
  else
    assert false (*TODO*)

