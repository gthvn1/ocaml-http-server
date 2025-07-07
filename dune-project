(lang dune 3.19)

(name olox_server)

(generate_opam_files true)

(package
 (name olox_server)
 (depends ocaml cohttp-lwt-unix conduit-lwt yojson astring))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
