(lang dune 3.16)

(name aoc)

(generate_opam_files true)

(source
 (github gustavomedeiross/aoc))

(authors "Gustavo Medeiros")

(maintainers "Gustavo Medeiros")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name aoc)
 (synopsis "Advent of Code")
 (description "Advent of Code implemntation in OCaml!")
 (depends ocaml dune ppx_deriving ppx_expect ppx_sexp_conv sexplib)
 (tags
  (topics "aoc" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
