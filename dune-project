(lang dune 3.19)

(name olox_server)
(maintainers "gthvn1@gmail.com")

(generate_opam_files true)

(package
 (name olox_server)
 (synopsis "An Interactive Coding Playground for the Lox Language")
 (depends ocaml cohttp-lwt-unix conduit-lwt yojson astring))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
