open Astring

type token_type = LeftParen | RightParen

let token_type_to_string = function
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"

type token = {
  token_type : token_type;
  lexeme : string;
  literal : string;
  line : int;
}

let token_to_string (tok : token) : string =
  Printf.sprintf "%s %s %s"
    (token_type_to_string tok.token_type)
    tok.lexeme tok.literal

type tokenizer = { source : string; tokens : token list; errors : string list }

let rec scan_tokens (tok : tokenizer) : tokenizer =
  Printf.printf ">> current string: %s\n%!" tok.source;
  match String.head tok.source with
  | None -> { tok with tokens = List.rev tok.tokens }
  | Some '(' ->
      scan_tokens
        {
          tok with
          source =
            String.sub ~start:1 ~stop:(String.length tok.source) tok.source
            |> String.Sub.to_string;
          tokens =
            { token_type = LeftParen; lexeme = "("; literal = "nil"; line = 1 }
            :: tok.tokens;
        }
  | Some c ->
      {
        tok with
        errors = ("Unkown character <" ^ String.of_char c ^ ">") :: tok.errors;
      }

let tokenize (str : string) : tokenizer =
  { source = str; tokens = []; errors = [] } |> scan_tokens

let tokens_to_string (toks : token list) : string =
  toks |> List.map token_to_string |> String.concat ~sep:"\n"
