
  - :exclamation: `max_length` replaced by `exceeds_max_length`
  - fixed build on 32-bit platforms e.g. js_of_ocaml (patch from Tim Bourke)

# 1.5 (30/01/2021)
  - new functions `rotatel` and `rotater` (patch from Tim Bourke)

# 1.4 (02/08/2020)
  - switch to dune build system, and opam 2.0
  - add to/of_bytes for machine-independant serialisation
  - fix binary serialisation when vector is larger than 2^32

# 1.3 (14/12/2017)
   - fix compilation for OCaml 4.06 (safe-string)

# 1.2 (10/02/2017)
   - added pop (population count, i.e., number of 1 bits)
   - suppressed some warnings with OCaml >= 4.00

# 1.1 (10/04/2013)
   - installation using ocamlfind (if found) or in `ocamlc -where`/bitv otherwise
   - fixed of_int64_(u)s on a 32-bit architecture (thanks to Florent Monnier)

# 1.0 (14/08/2012)
   - functions to/of_int64_s
   - new functions output_bin/input_bin for export/import in a
    machine-independent way (contribution from Bruno Guillaume)
   - fixed installation (patch from Bruno Guillaume)

# 0.9 (20/06/2012)
  - fixed tarball (test.ml was missing)
  - patched installation to use $(DESTDIR), if any (patch from Bruno Guillaume)

# 0.8 (28/02/2012)
  - two different modules L and M for conversions to and from string (least
   and most significant bits first, respectively); apologies for
   API incompatibility
  - improved implementation of iteri_true

# 0.7 (03/04/2008)
  - fixed bug with unsafe_blit when len=0 (could affect blit, sub, append,
   concat and fill when used with 0-length bit vectors), thanks to Pat Rondon

# 0.6 (18/02/2005)
  - added iteri_true
  - added foldi_left and foldi_right
  - patch for ocaml >= 3.08 (0xffffffff is no more accepted as a literal)
  - fixed Invalid_argument messages (now exactly the function name)

# 0.5 (22/11/2002)
  - added print : formatter -> t -> unit
  - from_list now fails on negative indexes (patch from Karl Zilles)
  - from_list_with_length to specify the length (patch from Karl Zilles)

# 0.4 (29/8/2002)
  - to_list/from_list to convert to/from lists of integers
  - added gray_iter to iterate over all bit vectors of length n
  - configuration with autoconf

# 0.3 (31/5/2000)
  - fixed bug in bw_not thanks to Damir Jamsek

# 0.2 (24/4/2000)
  - normalization, so that generic equality can be used
  - efficient implementations of append, sub, blit, concat and fill
  - documentation using ocamlweb
  - to_string, from_string
  - bitwise operations, all_zeros

# 0.1
  - first release
