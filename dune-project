(lang dune 3.6)
(name ansi)
(implicit_transitive_deps false)
(formatting (enabled_for dune))
(generate_opam_files true)

(source (github ocurrent/ansi))
(documentation "https://ocurrent.github.io/ansi/")
(maintainers
  "Antonin Décimo <antonin@tarides.com>"
  "Navin Keswani <navin@novemberkilo.io>"
  "<talex5@gmail.com>")
(authors
 "Antonin Décimo <antonin@tarides.com>"
 "talex5@gmail.com")
(license Apache-2.0)
(package
 (name ansi)
 (synopsis "ANSI escape sequence parser")
 (description "This package provides a basic ANSI escape parser, allowing the OCurrent web UI to show logs in colour.")
 (depends
  (ocaml (>= 4.10.0))
  astring
  (fmt (>= 0.8.7))
  tyxml
  (alcotest :with-test)))
