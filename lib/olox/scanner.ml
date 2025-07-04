open Astring

type tokenizer = {
  source : string;
  tokens : Token.t list;
  errors : string list;
}

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
