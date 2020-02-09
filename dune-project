(lang dune 1.11)
(name prelude)
(source (github mjustus/prelude))
(license BSD-3-clause)
(version 0.0.0)

(generate_opam_files true)

(package
 (name prelude)
 (synopsis "Haskell-style prelude")
 (depends
  (ocaml (>= 4.07.0))
  dune))

