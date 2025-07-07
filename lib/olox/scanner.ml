type tokenizer = {
  source : string;
  tokens : Token.t list;
  errors : string list;
  line : int;
}

let read_first_char (str : string) : char option * string =
  let open Astring in
  match String.head str with
  | None -> (None, str)
  | Some c ->
      ( Some c,
        String.sub ~start:1 ~stop:(String.length str) str
        |> String.Sub.to_string )

(* \n has its own case because we update a line *)
let is_whitespace = function ' ' | '\r' | '\t' -> true | _ -> false

let rec add_token tokizer rest token =
  scan_tokens { tokizer with source = rest; tokens = token :: tokizer.tokens }

and add_error tokizer rest err =
  scan_tokens { tokizer with source = rest; errors = err :: tokizer.errors }

and add_line tokizer rest =
  scan_tokens { tokizer with source = rest; line = tokizer.line + 1 }

and skip_char tokizer rest = scan_tokens { tokizer with source = rest }

and return_tokenizer tokizer =
  {
    tokizer with
    tokens = List.rev tokizer.tokens;
    errors = List.rev tokizer.errors;
  }

and scan_tokens (tok : tokenizer) : tokenizer =
  Printf.printf ">> current string: %s\n%!" tok.source;
  match read_first_char tok.source with
  | None, _ -> return_tokenizer tok
  | Some '\n', rest -> add_line tok rest
  | Some c, rest when is_whitespace c -> skip_char tok rest
  | Some '(', rest -> Token.of_char '(' tok.line |> add_token tok rest
  | Some ')', rest -> Token.of_char ')' tok.line |> add_token tok rest
  | Some c, rest ->
      "Unkown character <" ^ Astring.String.of_char c ^ "> at "
      ^ Astring.String.of_int tok.line
      |> add_error tok rest

let tokenize (str : string) : tokenizer =
  { source = str; tokens = []; errors = []; line = 1 } |> scan_tokens
