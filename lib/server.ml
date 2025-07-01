(* https://mirage.github.io/ocaml-conduit/conduit-lwt-unix/Conduit_lwt_unix/index.html *)

let port = 8000

(* Create a socket listening on given port. *)
let sock : Conduit_lwt_unix.tcp_config = `Port port

(* Listen on the specified TCPv4 port. *)
let listen : Conduit_lwt_unix.server = `TCP sock

let server_handler _conn req _body =
  let open Cohttp_lwt_unix in
  let met = req |> Request.meth |> Cohttp.Code.string_of_method in
  let hdr_lst = req |> Request.headers |> Cohttp.Header.to_list in

  (* Print some infos *)
  print_endline ("Method is " ^ met);
  print_endline "Header list: ";
  List.iter (fun (s1, s2) -> print_endline ("  " ^ s1 ^ " -> " ^ s2)) hdr_lst;

  (* and return OK *)
  let hello = `String "Hello, Sailor!" in
  Cohttp_lwt_unix.Server.respond ~status:`OK ~body:hello ()

let start_server =
  let server =
    Cohttp_lwt_unix.Server.(
      make ~callback:server_handler () |> create ~mode:listen)
  in
  print_endline ("Listenning on port " ^ string_of_int port);
  ignore (Lwt_main.run server)
