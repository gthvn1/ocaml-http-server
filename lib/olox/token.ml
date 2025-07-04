type token_type = LeftParen | RightParen

let token_type_to_string = function
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"

type t = {
  token_type : token_type;
  lexeme : string;
  literal : string;
  line : int;
}

let token_to_string (tok : t) : string =
  Printf.sprintf "%s %s %s"
    (token_type_to_string tok.token_type)
    tok.lexeme tok.literal

let tokens_to_string (toks : t list) : string =
  toks |> List.map token_to_string |> Astring.String.concat ~sep:"\n"
