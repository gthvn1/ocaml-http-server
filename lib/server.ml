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
  let path = req |> Request.resource in

  (* Print some infos about the request *)
  Printf.printf "Method is %s\n" met;
  Printf.printf "Path is %s\n" path;
  Printf.printf "Header list:\n";
  List.iter (fun (s1, s2) -> Printf.printf "  %s -> %s\n" s1 s2) hdr_lst;
  flush stdout;

  (* and return OK *)
  let body =
    match String.lowercase_ascii path with
    | "/joe" ->
        (* We want to keep upper/lower case from path so extract it. *)
        let name = String.sub path 1 3 in
        `String (Printf.sprintf "Hello, %s!" name)
    | _ -> `String "Hello, Sailor!"
  in
  Cohttp_lwt_unix.Server.respond ~status:`OK ~body ()

let start_server =
  let server =
    Cohttp_lwt_unix.Server.(
      make ~callback:server_handler () |> create ~mode:listen)
  in
  print_endline ("Listenning on port " ^ string_of_int port);
  ignore (Lwt_main.run server)
