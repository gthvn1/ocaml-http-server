(* https://mirage.github.io/ocaml-conduit/conduit-lwt-unix/Conduit_lwt_unix/index.html *)

let port = 8000
let static_index = "./static/index.html"
let static_css = "./static/style.css"
let static_favicon = "./static/favicon.ico"

(* Create a socket listening on given port. *)
let sock : Conduit_lwt_unix.tcp_config = `Port port

(* Listen on the specified TCPv4 port. *)
let listen : Conduit_lwt_unix.server = `TCP sock

let read_file name : string =
  let fname =
    match name with
    | "css" -> static_css
    | "favicon" -> static_favicon
    | _ -> static_index
  in
  let in_file = open_in fname in
  let sz = in_channel_length in_file in
  really_input_string in_file sz

let server_handler _conn req body =
  let open Cohttp_lwt_unix in
  let ( >>= ) = Lwt.bind in
  let met = req |> Request.meth |> Cohttp.Code.string_of_method in
  let hdr_lst = req |> Request.headers |> Cohttp.Header.to_list in
  let path = req |> Request.resource in

  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  (* Print some infos about the request *)
  Printf.printf "- Method: %s\n" met;
  Printf.printf "- Path: %s\n" path;
  Printf.printf "== Header list\n";
  List.iter (fun (s1, s2) -> Printf.printf "  - %s -> %s\n" s1 s2) hdr_lst;
  Printf.printf "== Body\n%s\n\n" body_str;
  flush stdout;

  (* and return OK *)
  let answer =
    match String.lowercase_ascii path with
    | "/joe" ->
        (* We want to keep upper/lower case from path so extract it. *)
        let name = String.sub path 1 3 in
        `String (Printf.sprintf "Hello, %s!" name)
    | "/clicked" -> `String "Whoa, you actually clicked it!"
    | "/lox" -> `String "Not yet implemented"
    | "/favicon.ico" -> `String (read_file "favicon")
    | "/style.css" -> `String (read_file "css")
    | _ -> `String (read_file "_index")
  in
  Cohttp_lwt_unix.Server.respond ~status:`OK ~body:answer ()

let start_server =
  let server =
    Cohttp_lwt_unix.Server.(
      make ~callback:server_handler () |> create ~mode:listen)
  in
  print_endline ("Listenning on port " ^ string_of_int port);
  ignore (Lwt_main.run server)
