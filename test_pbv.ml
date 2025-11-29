
(* persistent bit vectors *)

open Bitv__Pbv

(* test Native = bit vectors of size [Sys.int_size] *)
let () =
  let open Native in
  let empty = empty Sys.int_size in
  let full = full Sys.int_size in
  let max_value = Sys.int_size - 1 in
  assert (cardinal full = Sys.int_size);
  assert (max_elt full = Sys.int_size - 1);
  assert (max_elt full = max_value);
  assert (min_elt full = 0);
  assert (union full full = full);
  assert (union empty full = full);
  assert (inter full full = full);
  assert (inter full empty = empty);
  assert (diff empty full = empty);
  assert (diff full empty = full);
  assert (disjoint empty full);
  assert (find_first (fun x -> x > 10) full = 11);
  assert (find_last (fun x -> x > 10) full = Sys.int_size - 1);
  let () =
    let f i = if i mod 2 = 0 && i >= 2 then Some (i-1) else None in
    let s = filter_map f full in
    assert (cardinal s = Sys.int_size / 2);
    for i = 0 to Sys.int_size - 1 do
      if i land 1 = 1 then assert (mem i s) else assert (not (mem i s))
    done
  in
  let testl l =
    let s = List.fold_left (fun s x -> add x s) empty l in
    assert (cardinal s = List.length l);
    List.iter (fun x -> assert (mem x s)) l;
    assert (min_elt s = List.fold_left min (List.hd l) l);
    assert (max_elt s = List.fold_left max (List.hd l) l);
    assert (of_seq (List.to_seq l) = s);
    assert (inter s s = s);
    assert (union s s = s);
    assert (diff s s = empty);
    assert (disjoint empty s);
    assert (List.of_seq (to_seq s) = l);
    assert (List.of_seq (to_rev_seq s) = List.rev l);
    ()
  in
  testl [1; 3; 8; 11];
  testl [2; 3; 4; 5];
  testl [62];
  testl [0; 62];
  testl [60; 61; 62];
  assert (to_seq empty () = Seq.Nil);
  assert (to_rev_seq empty () = Seq.Nil);
  ()

(* test any implementation, with a given size *)
let test (module X: S) (size: int) =
  Format.printf "size = %d@." size;
  let v0 = X.make size false in
  (* Format.printf "  v0 = %a@." X.print v0; *)
  assert (X.length v0 = size);
  assert (X.is_empty v0);
  assert (X.pop v0 = 0);
  let v1 = X.make size true in
  (* Format.printf "  v1 = %a@." X.print v1; *)
  assert (X.length v1 = size);
  assert (X.pop v1 = size);
  assert (X.nlz v1 = 0);
  assert (X.ntz v1 = 0);
  for i = 0 to size - 1 do
    (* Format.printf "  i = %d@." i; *)
    let b = X.set v0 i true in
    (* Format.printf "    b = %a@." X.print b; *)
    assert (X.length b = size);
    assert (X.get b i);
    assert (X.pop b = 1);
    assert (X.ntz b = i);
    assert (b = X.singleton size i);
    assert (X.swap b i = v0);
    let v = X.set v1 i false in
    assert (X.length v = size);
    assert (not (X.get v i));
    assert (X.pop v = size-1);
    (* Format.printf "    v = %a@." X.print v; *)
    (* Format.printf "    U = %a@." X.print (X.union b v); *)
    assert (X.union b v = v1);
    assert (X.inter b v = v0);
    assert (X.diff v1 b = v);
    assert (X.swap v i = v1);
    let s = X.singleton size i in
    assert (X.cardinal s = 1);
    assert (X.min_elt s = i);
    assert (X.max_elt s = i);
  done;
  (*** Eratosthene's sieve *)
  let sieve (limit: int) =
    assert (limit > 1);
    let rec loop v n =
      if n > limit then v else
      if X.unsafe_get v n then (* n is prime *)
        let rec mark v i =
          if i > limit then v else
          let v = X.unsafe_set v i false in mark v (i + 2*n) in
        let v = if n <= limit/n then mark v (n * n) else v in
        loop v (n + 2)
      else
        loop v (n + 2) in
    let v = X.init (limit + 1) (fun i -> i >= 2 && (i = 2 || i mod 2 = 1)) in
    loop v 3
  in
  if size >= 101 then assert (X.pop (sieve 100) = 25);
  if size >= 1001 then assert (X.pop (sieve 1000) = 168);
  (*****)
  for _ = 1 to 10 do
    let i = Random.int size in
    let v = X.singleton size i in
    assert (X.pop v = 1);
    assert (X.find_first (fun j -> j >= i) v = i);
    assert (List.of_seq (X.to_seq v) = [i]);
  done;
  if size >= 10 then (
    let v = X.init size (fun i -> i < 10) in
    assert (X.foldi_true (+) v 0 = 45)
  );
  ()

let () = test (module Native) Sys.int_size
let () = test (module Large) 31
let () = test (module Large) 32
let () = test (module Large) Sys.int_size
let () = test (module Large) 200
let () = test (module Large) 1100

let () =
  let open Large in
  let v = init 10 (fun _ -> true) in
  assert (foldi_true (+) v 0 = 45);
  ()

