(lang dune 2.9)

(cram enable)

(name compiler)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name compiler)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
