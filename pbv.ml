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

module type SET = sig
  type elt
  type t
  val empty: int -> t
  val is_empty: t -> bool
  val full: int -> t
  val mem: elt -> t -> bool
  val cardinal: t -> int (* same as pop *)
  val singleton: int -> elt -> t
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
  val iter_subsets: (t -> unit) -> t -> unit
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
end

module type S = sig
  type t
  val max_length: int
  val length: t -> int
  val make: int -> bool -> t
  val init: int -> (int -> bool) -> t
  val get: t -> int -> bool
  val set: t -> int -> bool -> t
  val iteri: (int -> bool -> unit) -> t -> unit
  val foldi: (int -> bool -> 'a -> 'a) -> t -> 'a -> 'a

  val swap: t -> int -> t
  val bw_and: t -> t -> t
  val bw_or: t -> t -> t
  val bw_xor: t -> t -> t
  val bw_not: t -> t
  val pop: t -> int
  val ntz: t -> int
  val nlz: t -> int
  val print: Format.formatter -> t -> unit

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int

  val unsafe_get: t -> int -> bool
  val unsafe_set: t -> int -> bool -> t

  include SET with type elt = int and type t := t
end

let print_list_as_set print fmt l =
  let rec pr = function
    | [] -> ()
    | x :: l ->
        print fmt x; if l <> [] then Format.fprintf fmt ",@,";
        pr l
  in
  Format.fprintf fmt "{";
  pr l;
  Format.fprintf fmt "}"

module SetOps(X: sig
  type t
  val make: int -> bool -> t
  val length: t -> int
  val is_empty: t -> bool
  val pop: t -> int
  val ntz: t -> int
  val nlz: t -> int
  val get: t -> int -> bool
  val unsafe_get: t -> int -> bool
  val set: t -> int -> bool -> t
  val unsafe_set: t -> int -> bool -> t
  val bw_and: t -> t -> t
  val bw_or: t -> t -> t
  val bw_xor: t -> t -> t
  val bw_not: t -> t
  val elements: t -> int list
end) = struct
  let empty size = X.make size false
  let full size = X.make size true
  let mem i v = X.get v i
  let cardinal = X.pop
  let find i s = if mem i s then i else raise Not_found
  let find_opt i s = if mem i s then Some i else None
  let union = X.bw_or
  let inter = X.bw_and
  let diff v1 v2 = X.bw_and v1 (X.bw_not v2)
  let subset v1 v2 = X.is_empty (X.bw_and v1 (X.bw_not v2))
  let disjoint v1 v2 = X.is_empty (X.bw_and v1 v2)
  let min_elt v =
    if X.is_empty v then invalid_arg "min_elt";
    X.ntz v
  let min_elt_opt v =
    if X.is_empty v then None else Some (X.ntz v)
  let max_elt v =
    if X.is_empty v then invalid_arg "min_elt";
    X.length v - 1 - X.nlz v
  let max_elt_opt v =
    if X.is_empty v then None else Some (Sys.int_size - 1 - X.nlz v)
  let choose = min_elt
  let choose_opt = min_elt_opt
  let check_index s v i =
    if i < 0 || i >= X.length v then invalid_arg s
  let add i v =
    check_index "add" v i;
    X.set v i true
  let remove i v =
    check_index "remove" v i;
    X.set v i false
  let print fmt v =
    for i = X.length v - 1 downto 0 do
      Format.fprintf fmt "%c" (if X.get v i then '1' else '0')
    done

  let find_first p v =
    let rec loop v =
      if X.is_empty v then raise Not_found;
      let x = min_elt v in
      if p x then x else loop (X.unsafe_set v x false) in
    loop v

  let find_first_opt p v =
    try Some (find_first p v) with Not_found -> None

  let find_last p v =
    let rec loop v =
      if X.is_empty v then raise Not_found;
      let x = max_elt v in
      if p x then x else loop (X.unsafe_set v x false) in
    loop v

  let find_last_opt p v =
    try Some (find_last p v) with Not_found -> None

  let rec for_all p v =
    X.is_empty v ||
    let x = min_elt v in p x && for_all p (X.unsafe_set v x false)

  let rec exists p v =
    not (X.is_empty v) &&
    let x = min_elt v in p x || exists p (X.unsafe_set v x false)

  let rec filter p v =
    if X.is_empty v then
      empty (X.length v)
    else
      let x = min_elt v in
      let v = filter p (X.unsafe_set v x false) in
      if p x then add x v else v

  let rec filter_map f v =
    if X.is_empty v then
      empty (X.length v)
    else
      let x = min_elt v in
      let v = filter_map f (X.unsafe_set v x false) in
      match f x with
      | None -> v
      | Some x -> add x v

  let rec partition p v =
     if X.is_empty v then
       let v = empty (X.length v) in v, v
    else
      let x = min_elt v in
      let vt,vf = partition p (X.unsafe_set v x false) in
      if p x then add x vt, vf else vt, add x vf

  let split x v =
    filter (fun y -> y < x) v, X.get v x, filter (fun y -> y > x) v

  let print_set fmt s =
    print_list_as_set Format.pp_print_int fmt (X.elements s)

  let of_list l =
    List.fold_left (fun s x -> add x s) (empty (List.length l)) l

  let of_seq s =
    Seq.fold_left (fun v x -> add x v) (empty (Seq.length s)) s

  let rec to_seq_from x v =
    if x > max_elt v then Seq.empty
    else if mem x v then fun () -> Seq.Cons (x, to_seq_from (x + 1) v)
    else to_seq_from (x + 1) v

  let to_seq v =
    if X.is_empty v then Seq.empty else to_seq_from (min_elt v) v

  let rec to_rev_seq_from x v =
    if x < min_elt v then Seq.empty
    else if mem x v then fun () -> Seq.Cons (x, to_rev_seq_from (x - 1) v)
    else to_rev_seq_from (x - 1) v

  let to_rev_seq v =
    if X.is_empty v then Seq.empty else to_rev_seq_from (max_elt v) v

  let rec add_seq veq v = match veq () with
    | Seq.Nil -> v
    | Seq.Cons (x, veq) -> add_seq veq (add x v)

end

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

let compute_ntz size x =
  if x = 0 then size else tib (x land (-x))

let compute_nlz size x =
  if x == 0 then size else
  let rec loop i = if x land i != 0 then size - 1 - tib i else loop (i lsr 1) in
  loop (1 lsl (size - 1))

module Small(X: sig val size: int end) = struct

  let () = if X.size < 0 || X.size > Sys.int_size then invalid_arg "Small"

  type t = int (* including the sign bit *)

  let compare = Int.compare
  let equal = (==)
  let hash v = v

  type elt = int

  let max_length =
    X.size

  let length _v =
    X.size

  let make _n b =
    if b then 1 lsl X.size - 1 else 0

  let init _n f =
    let rec build v i = if i < 0 then v else
      let v = if f i then (v lsl 1) lor 1 else v lsl 1 in build v (i-1) in
    build 0 (X.size - 1)

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

  let iteri f v =
    let n = length v in
    let rec loop i = if i < n then (f i (unsafe_get v i); loop (i+1)) in
    loop 0

  let foldi f v acc =
    let n = length v in
    let rec loop i acc =
      if i < n then loop (i+1) (f i (unsafe_get v i) acc) else acc in
    loop 0 acc

  let swap v i =
    check_index "swap" v i;
    v lxor (1 lsl i)

  let bw_or  = (lor)
  let bw_and = (land)
  let bw_xor = (lxor)
  let bw_not v = (lnot v) land (1 lsl X.size - 1)

  let ntz v = compute_ntz max_length v
  let nlz v = compute_nlz max_length v
  let pop = pop

  let singleton len i =
    if i < 0 || i >= len then invalid_arg "singleton";
    1 lsl i
  let is_empty v =
    v == 0

  let rec elements v =
    if v == 0 then [] else let i = v land (-v) in tib i :: elements (v - i)

  include SetOps(struct
    type t_ = t type t = t_
    let make = make
    let length = length
    let is_empty = is_empty
    let pop = pop
    let ntz = ntz
    let nlz = nlz
    let get = get
    let unsafe_get = unsafe_get
    let set = set
    let unsafe_set = unsafe_set
    let bw_or = bw_or
    let bw_and = bw_and
    let bw_xor = bw_xor
    let bw_not = bw_not
    let elements = elements
  end)

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

  let rec iteri_true f v =
    if v != 0 then let i = v land (-v) in f (tib i); iteri_true f (v - i)
  let rec iteri_true_ofs f ofs v =
    if v != 0 then
      let i = v land (-v) in f (ofs + tib i); iteri_true_ofs f ofs (v - i)

  let rec foldi_true f v acc =
    if v == 0 then acc else let i = v land (-v) in foldi_true f (v - i) (f (tib i) acc)
  let rec foldi_true_ofs f ofs v acc =
    if v == 0 then acc else
    let i = v land (-v) in foldi_true_ofs f ofs (v - i) (f (ofs + tib i) acc)

  let iter_subsets f v =
    let rec iter s v =
      if v = 0 then f s else (
      let b = v land (-v) in let v = v - b in iter (s + b) v; iter s v) in
    iter 0 v

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

module Native = Small(struct let size = Sys.int_size end)

module Large : S = struct

  type elt = int

  (* rope-like data structure, with leaves for BV of size <= 32 and binary
     nodes otherwise *)
  type t =
    | Leaf of int
    | Node of { info: int; high: t; low: t }

  (* the representation is unique *)
  let compare: t -> t -> int = Stdlib.compare
  let equal: t -> t -> bool = (=)
  let hash: t -> int = Hashtbl.hash

  (* leaf
         62     56     50     44     38     32 31                           0
         +-+------+------+------+------+------+------------------------------+
         |?|  ??  | nlz  | ntz  | pop  | size |            bits              |
         +-+------+------+------+------+------+------------------------------+
  *)

  let bits x = x land 0xFFFF_FFFF
  let ilen x = (x lsr 32) land 0x3F
  let ipop x = (x lsr 38) land 0x3F
  let intz x = (x lsr 44) land 0x3F
  let inlz x = (x lsr 50) land 0x3F
  let imk ~nlz ~ntz ~pop ~size bits =
    assert (bits < 0x1_0000_0000);
    assert (bits lsr size = 0);
    (nlz lsl 50) lor (ntz lsl 44) lor (pop lsl 38) lor (size lsl 32) lor bits
  let ibits size bits =
    imk ~nlz:(compute_nlz size bits) ~ntz:(compute_ntz size bits)
        ~pop:(pop bits) ~size bits
  let iget x i =
    (x lsr i) land 1 <> 0
  let iswap x i =
    let b = iget x i in
    let bits = bits x in
    let bits = if b then bits land (lnot (1 lsl i)) else bits lor (1 lsl i) in
    let pop = if b then ipop x - 1 else ipop x + 1 in
    let size = ilen x in
    let nlz = compute_nlz size bits in
    let ntz = compute_ntz size bits in
    imk ~nlz ~ntz ~pop ~size bits
  let ibw_and x y =
    let size = ilen x in assert (ilen y = size); ibits size (bits x land bits y)
  let ibw_or x y =
    let size = ilen x in assert (ilen y = size); ibits size (bits x lor  bits y)
  let ibw_xor x y =
    let size = ilen x in assert (ilen y = size); ibits size (bits x lxor bits y)
  let ibw_not x =
    let size = ilen x in ibits size ((lnot (bits x)) land (1 lsl size - 1))
  let iinit size f =
    let rec build v i = if i < 0 then ibits size v else
      let v = if f i then (v lsl 1) lor 1 else v lsl 1 in build v (i-1) in
    build 0 (size - 1)

  (* pre-allocated 0 and 1 leaves for every size from 0 to 32 *)
  let izeros =
    Array.init 33 (fun size -> Leaf (imk ~nlz:size ~ntz:size ~pop:0 ~size 0))
  let iones =
    Array.init 33 (fun size -> Leaf (imk ~nlz:0 ~ntz:0 ~pop:size ~size (1 lsl size - 1)))
  let imake size b =
    if b then iones.(size) else izeros.(size)

  (* binary node info
         62 61                              31 30                           0
         +-+----------------------------------+------------------------------+
         |?|             ntz                  |            size              |
         +-+----------------------------------+------------------------------+

  *)

  let nlen i = i land 0x7FFF_FFFF
  let nntz i = i lsr 31

  let max_length = 1 lsl 31 - 1

  let length = function
    | Leaf x        -> ilen x
    | Node {info;_} -> nlen info

  let ntz = function
    | Leaf x        -> intz x
    | Node {info;_} -> nntz info

  let node ~high ~low =
    let lenl = length low and lenh = length high in
    let len = lenl + lenh in
    if len > max_length then invalid_arg "max length exceeded";
    let ntzl = ntz low and ntzh = ntz high in
    let ntz = if ntzl < lenl then ntzl else lenl + ntzh in
    let info = (ntz lsl 31) lor len in
    Node { info; high; low }

  (* We have constant time access to [length] and [ntz], and thus
     constant time functions [is_empty] and [min_elt]. *)

  let is_empty v =
    ntz v = length v

  (* returns both size and size+1 *)
  (* FIXME preallocate size 33 *)
  let rec make2 size b =
    if size < 32 then imake size b, imake (size+1) b else
    if size = 32 then imake 32 b, node ~high:(imake 1 b) ~low:(imake 32 b) else
    let vn, vn1 = make2 (size / 2) b in
    if size mod 2 = 0 then node ~high:vn ~low:vn , node ~high:vn  ~low:vn1
                      else node ~high:vn ~low:vn1, node ~high:vn1 ~low:vn1

  (* FIXME specialized `make` for powers of two *)
  let make size b =
    let v, _ = make2 size b in v

  let rec init size f =
    if size <= 32 then Leaf (iinit size f) else
    let lh = size / 2 in
    let ll = size - lh in
    node ~high:(init lh (fun i -> f (ll + i))) ~low:(init ll f)

  let rec unsafe_get v i = match v with
    | Leaf x ->
        iget x i
    | Node {high; low; _} ->
        let ll = length low in
        if i < ll then unsafe_get low i else unsafe_get high (i - ll)

  let unsafe_get v i =
    if is_empty v then false else unsafe_get v i

  let rec unsafe_set v i b = match v with
    | Leaf x ->
        if iget x i = b then v else Leaf (iswap x i)
    | Node { high; low; _ } ->
        let ll = length low in
        if i < ll then node ~high ~low:(unsafe_set low i b)
                  else node ~high:(unsafe_set high (i-ll) b) ~low

  let check_index s v i =
    if i < 0 || i >= length v then invalid_arg s

  let get v i =
    check_index "get" v i;
    unsafe_get v i

  let set v i b =
    check_index "set" v i;
    unsafe_set v i b

  let rec pop = function
    | Leaf x -> ipop x
    | Node { high; low; _ } -> pop high + pop low

  let check_same_size s v1 v2 =
    if length v1 <> length v2 then invalid_arg s

  let rec bw_op iop v1 v2 = match v1, v2 with
    | Leaf x1, Leaf x2 -> Leaf (iop x1 x2)
    | Node {high=h1;low=l1;_}, Node {high=h2;low=l2;_} ->
        node ~high:(bw_op iop h1 h2) ~low:(bw_op iop l1 l2)
    | _ -> assert false
  let bw_and v1 v2 =  check_same_size "bw_and" v1 v2; bw_op ibw_and v1 v2
  let bw_or  v1 v2 =  check_same_size "bw_or"  v1 v2; bw_op ibw_or  v1 v2
  let bw_xor v1 v2 =  check_same_size "bw_xor" v1 v2; bw_op ibw_xor v1 v2
  let rec bw_not = function
    | Leaf x -> Leaf (ibw_not x)
    | Node {high;low;_} -> node ~high:(bw_not high) ~low:(bw_not low)

  let rec swap v i = match v with
    | Leaf x -> Leaf (iswap x i)
    | Node  { high; low; _ } ->
        let ll = length low in
        if i < ll then node ~high ~low:(swap low i)
                  else node ~high:(swap high (i-ll)) ~low

  let rec nlz = function
    | Leaf x -> inlz x
    | Node { high; low; _ } ->
        let nlzh = nlz high in
        if nlzh < length high then nlzh else nlzh + nlz low

  (* FIXME: improve *)
  let iteri f v =
    let n = length v in
    let rec loop i = if i < n then (f i (unsafe_get v i); loop (i+1)) in
    loop 0

  (* FIXME: improve *)
  let foldi f v acc =
    let n = length v in
    let rec loop i acc =
      if i < n then loop (i+1) (f i (unsafe_get v i) acc) else acc in
    loop 0 acc

  let elements v =
    let rec elements acc ofs = function
      | Leaf x ->
          let rec loop acc x =
            if x == 0 then acc else
            let i = x land (-x) in loop (ofs + tib i :: acc) (x - i) in
          loop acc (bits x)
      | Node {high;low;_} ->
          let ll = length low in
          elements (elements acc ofs low) (ll + ofs) high
    in
    elements [] 0 v

  include SetOps(struct
    type t_ = t type t = t_
    let make = make
    let length = length
    let is_empty = is_empty
    let pop = pop
    let ntz = ntz
    let nlz = nlz
    let get = get
    let unsafe_get = unsafe_get
    let set = set
    let unsafe_set = unsafe_set
    let bw_or = bw_or
    let bw_and = bw_and
    let bw_xor = bw_xor
    let bw_not = bw_not
    let elements = elements
  end)

  let singleton size i =
    set (make size false) i true

  let iteri_true f v =
    let rec iter ofs = function
    | Leaf x ->
        Native.iteri_true_ofs f ofs (bits x)
    | Node {high;low;info} ->
        let ll = length low in
        if nntz info < ll then iter ofs low;
        if not (is_empty high) then iter (ofs+ll) high
    in
    iter 0 v

  let foldi_true f v acc =
    let rec fold ofs acc = function
    | Leaf x ->
        Native.foldi_true_ofs f ofs (bits x) acc
    | Node {high;low;info} ->
        let ll = length low in
        let acc = if nntz info < ll then fold ofs acc low else acc in
        if not (is_empty high) then fold (ofs+ll) acc high else acc
    in
    fold 0 acc v

  let iter_subsets _f _v =
    assert false (*TODO*)

end

(* TODO: module FixedSize for large bit vectors with a fixed size *)

let fixed_size n : (module S) =
  if n = Sys.int_size then
    (module Native)
  else if n < Sys.int_size then
    (module Small(struct let size = n end))
  else
    assert false (*TODO*)

module type UNIVERSE = sig
  type t
  val hash: t -> int
  val equal: t -> t -> bool
  val print: Format.formatter -> t -> unit
end

module Make(X: UNIVERSE) = struct
  let create ?(unsafe=false) elements =
    let module H = Hashtbl.Make(X) in
    let n = List.length elements in
    if n = 0 then invalid_arg "create: empty list of elements";
    let toint_ : int H.t = H.create n in
    let ofint_ = Array.make n (List.hd elements) in
    let next = ref 0 in
    let add x =
      if H.mem toint_ x then invalid_arg "create: duplicate element";
      let i = !next in incr next; H.add toint_ x i; ofint_.(i) <- x in
    List.iter add elements;
    assert (!next = n);
    let toint =
      if unsafe then H.find toint_ else
      fun x -> try H.find toint_ x with Not_found ->
                 Format.kasprintf invalid_arg "not an element (%a)" X.print x in
    let ofint (i: int) : X.t = Array.unsafe_get ofint_ i in
    let module S = struct
      module M = (val fixed_size n)
      type elt = X.t
      type t = M.t
      let is_empty = M.is_empty
      let empty _ = M.empty n
      let full _ = M.full n
      let add x s = M.add (toint x) s
      let mem x s = M.mem (toint x) s
      let cardinal = M.cardinal
      let singleton _ x = toint x |> M.singleton n
      let min_elt s = M.min_elt s |> ofint
      let max_elt s = M.max_elt s |> ofint
      let min_elt_opt s = M.min_elt_opt s |> Option.map ofint
      let max_elt_opt s = M.max_elt_opt s |> Option.map ofint
      let remove x s =  M.remove (toint x) s
      let union = M.union
      let inter = M.inter
      let diff = M.diff
      let subset = M.subset
      let disjoint = M.disjoint
      let transpose f x = f (ofint x)
      let iteri_true f s = M.iteri_true (transpose f) s
      let foldi_true f s acc = M.foldi_true (fun x acc -> f (ofint x) acc) s acc
      let iter_subsets = M.iter_subsets
      let for_all f s =  M.for_all (transpose f) s
      let exists f s =  M.exists (transpose f) s
      let filter f s = M.filter (transpose f) s
      let filter_map f s = M.filter_map (fun x -> Option.map toint (f (ofint x))) s
      let partition f s = M.partition (transpose f) s
      let elements s = List.map ofint (M.elements s)
      let choose s = M.choose s |> ofint
      let choose_opt s = M.choose_opt s  |> Option.map ofint
      let split x s = M.split (toint x) s
      let find x s = M.find (toint x) s |> ofint
      let find_opt x s = M.find_opt (toint x) s |> Option.map ofint
      let find_first f s = M.find_first (transpose f) s |> ofint
      let find_first_opt f s = M.find_first_opt (transpose f) s |> Option.map  ofint
      let find_last f s = M.find_last (transpose f) s |> ofint
      let find_last_opt f s = M.find_last_opt (transpose f) s |> Option.map  ofint
      let of_list l = List.map toint l |> M.of_list
      let to_seq_from x s = M.to_seq_from (toint x) s |> Seq.map ofint
      let to_seq s = M.to_seq s |> Seq.map ofint
      let to_rev_seq s = M.to_rev_seq s |> Seq.map ofint
      let add_seq sq s = M.add_seq (Seq.map toint sq) s
      let of_seq sq = Seq.map toint sq |> M.of_seq
      let print_set fmt s = print_list_as_set X.print fmt (elements s)
    end in
    (module S : SET with type elt = X.t)
end
