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

(* [is_digit c] returns true if c is a digit *)
let is_digit (c : char) : bool = c >= '0' && c <= '9'

let is_alpha (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* prepend a character to a string *)
let prepend_char s c = s ^ Astring.String.of_char c

let rec add_token tokizer rest token =
  scan_tokens { tokizer with source = rest; tokens = token :: tokizer.tokens }

and add_error tokizer rest err =
  scan_tokens { tokizer with source = rest; errors = err :: tokizer.errors }

and add_line tokizer rest =
  scan_tokens { tokizer with source = rest; line = tokizer.line + 1 }

and add_token_from_string s tokizer rest =
  Token.of_string s tokizer.line |> add_token tokizer rest

and skip_char tokizer rest = scan_tokens { tokizer with source = rest }

and skip_until_eol tokizer =
  match read_first_char tokizer.source with
  | None, _ -> return_tokenizer tokizer
  | Some '\n', rest ->
      scan_tokens { tokizer with source = rest; line = tokizer.line + 1 }
  | Some _, rest -> skip_until_eol { tokizer with source = rest }

and gen_string_token tokizer str rest =
  scan_tokens
    {
      tokizer with
      source = rest;
      tokens = Token.create_string_token str tokizer.line :: tokizer.tokens;
    }

(* This is called when we already read a quote. So we need to find the closing
   one. *)
and read_string_literals tokizer str =
  match read_first_char tokizer.source with
  | None, _ ->
      return_tokenizer
        { tokizer with errors = "Unterminated string" :: tokizer.errors }
  | Some '"', rest -> gen_string_token tokizer str rest
  | Some '\n', rest ->
      read_string_literals
        { tokizer with source = rest; line = tokizer.line + 1 }
        (prepend_char str '\n')
  | Some c, rest ->
      read_string_literals { tokizer with source = rest } (prepend_char str c)

and gen_number_token tokizer str =
  let token = Token.create_number_token str tokizer.line in
  scan_tokens { tokizer with tokens = token :: tokizer.tokens }

and read_number tokizer value decimal_part =
  match read_first_char tokizer.source with
  | Some c, rest when is_digit c ->
      read_number
        { tokizer with source = rest }
        (prepend_char value c) decimal_part
  | Some '.', rest ->
      if decimal_part then gen_number_token tokizer value
      else
        read_number { tokizer with source = rest } (prepend_char value '.') true
  | _ -> gen_number_token tokizer value

and gen_keyword_token tokizer keyword =
  let token = Token.create_keyword_token keyword tokizer.line in
  scan_tokens { tokizer with tokens = token :: tokizer.tokens }

and read_identifier tokizer keyword =
  match read_first_char tokizer.source with
  | Some c, rest when is_alpha c ->
      read_identifier { tokizer with source = rest } (prepend_char keyword c)
  | _ -> gen_keyword_token tokizer keyword

and return_tokenizer tokizer =
  {
    tokizer with
    tokens = List.rev (Token.eof_token tokizer.line :: tokizer.tokens);
    errors = List.rev tokizer.errors;
  }

and scan_tokens (tok : tokenizer) : tokenizer =
  Printf.printf ">> current string: %s\n%!" tok.source;
  match read_first_char tok.source with
  | None, _ -> return_tokenizer tok
  | Some '\n', rest -> add_line tok rest
  | Some c, rest when is_whitespace c -> skip_char tok rest
  | Some '(', rest -> add_token_from_string "(" tok rest
  | Some ')', rest -> add_token_from_string ")" tok rest
  | Some '{', rest -> add_token_from_string "{" tok rest
  | Some '}', rest -> add_token_from_string "}" tok rest
  | Some ',', rest -> add_token_from_string "," tok rest
  | Some '.', rest -> add_token_from_string "." tok rest
  | Some '-', rest -> add_token_from_string "-" tok rest
  | Some '+', rest -> add_token_from_string "+" tok rest
  | Some ';', rest -> add_token_from_string ";" tok rest
  | Some '*', rest -> add_token_from_string "*" tok rest
  | Some '/', rest -> (
      match read_first_char rest with
      | Some '/', rest' -> skip_until_eol { tok with source = rest' }
      | _ -> add_token_from_string "/" tok rest)
  | Some '!', rest -> (
      match read_first_char rest with
      | Some '=', rest' -> add_token_from_string "!=" tok rest'
      | _ -> add_token_from_string "!" tok rest)
  | Some '=', rest -> (
      match read_first_char rest with
      | Some '=', rest' -> add_token_from_string "==" tok rest'
      | _ -> add_token_from_string "=" tok rest)
  | Some '>', rest -> (
      match read_first_char rest with
      | Some '=', rest' -> add_token_from_string ">=" tok rest'
      | _ -> add_token_from_string ">" tok rest)
  | Some '<', rest -> (
      match read_first_char rest with
      | Some '=', rest' -> add_token_from_string "<=" tok rest'
      | _ -> add_token_from_string "<" tok rest)
  | Some '"', rest -> read_string_literals { tok with source = rest } ""
  | Some c, _ when is_digit c -> read_number tok "" false
  | Some c, _ when is_alpha c -> read_identifier tok ""
  | Some c, rest ->
      "Unkown character <" ^ Astring.String.of_char c ^ "> at "
      ^ Astring.String.of_int tok.line
      |> add_error tok rest

let tokenize (str : string) : tokenizer =
  { source = str; tokens = []; errors = []; line = 1 } |> scan_tokens
