
(* persistent bit vectors *)

open Bitv__Pbv

let test (module X: S) (size: int) =
  let v0 = X.make size false in
  assert (X.length v0 = size);
  let v1 = X.make size true in
  assert (X.length v1 = size);
  for i = 0 to size - 1 do
    let v = X.set v0 i true in
    assert (X.length v = size);
    assert (X.get v i);
    (* assert (X.pop v = 1); *)
    let v = X.swap v i in
    assert (v = v0);
    let v = X.set v1 i false in
    assert (X.length v = size);
    assert (not (X.get v i));
    (* assert (X.pop v = size-1); *)
    let v = X.swap v i in
    assert (v = v1);
  done;
  ()

let () = test (module Native) Sys.int_size

