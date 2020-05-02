(lang dune 2.5)
(name discord-ml)

(generate_opam_files true)

(package
 (name discord-ml)
 (synopsis "An OCaml library to build Discord bot")
 (description "TODO long decription")
 (depends
  (lwt (>= 5.3.0))
  (cohttp (>= 2.5.1))
  (cohttp-lwt-unix (>= 2.5.1))
  (websocket (>= 2.14))
  (websocket-lwt (>= 2.12))
  (websocket-lwt-unix (>= 2.14))
  (logs (>= 1.7.0))
  (yojson (>= 1.7.0))
  (ppx_deriving_yojson (>= 3.5.2))))