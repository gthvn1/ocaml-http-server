(* https://mirage.github.io/ocaml-conduit/conduit-lwt-unix/Conduit_lwt_unix/index.html *)

let port = 8000

type file_type = Index | Css | Icon

let static_index = "./static/index.html"
let static_css = "./static/style.css"
let static_favicon = "./static/favicon.ico"

(* Create a socket listening on given port. *)
let sock : Conduit_lwt_unix.tcp_config = `Port port

(* Listen on the specified TCPv4 port. *)
let listen : Conduit_lwt_unix.server = `TCP sock

let read_file (ft : file_type) : string =
  let fname =
    match ft with
    | Css -> static_css
    | Icon -> static_favicon
    | Index -> static_index
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
    | "/olox" ->
        let toks = Olox.tokenize body_str in
        let ast =
          match Olox.parse toks.tokens with
          | Error s -> s
          | Ok ast -> ast |> Olox.ast_to_string
        in
        let resp =
          `Assoc
            [
              ("tokens", `String (Olox.tokens_to_string toks.tokens));
              ("ast", `String ast);
              ("errors", `String (String.concat "\n" toks.errors));
            ]
        in
        `String (Yojson.Safe.to_string resp)
    | "/favicon.ico" -> `String (read_file Icon)
    | "/style.css" -> `String (read_file Css)
    | _ -> `String (read_file Index)
  in
  Cohttp_lwt_unix.Server.respond ~status:`OK ~body:answer ()

let start_server =
  print_endline ("Listenning on port " ^ string_of_int port);
  Cohttp_lwt_unix.Server.(
    make ~callback:server_handler () |> create ~mode:listen)
  |> Lwt_main.run
