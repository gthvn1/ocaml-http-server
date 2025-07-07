type token_type =
  | (* Single-character tokens. *)
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Eof

let token_type_to_string = function
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Minus -> "Minus"
  | Plus -> "Plus"
  | Semicolon -> "Semicolon"
  | Slash -> "Slash"
  | Star -> "Star"
  | Eof -> "Eof"

type t = {
  token_type : token_type;
  lexeme : string;
  literal : string;
  line : int;
}

let eof_token line = { token_type = Eof; lexeme = ""; literal = "nil"; line = line }

let of_char (c : char) (line : int) : t =
  match c with
  | '(' -> { token_type = LeftParen; lexeme = "("; literal = "nil"; line }
  | ')' -> { token_type = RightParen; lexeme = ")"; literal = "nil"; line }
  | '{' -> { token_type = LeftBrace; lexeme = "{"; literal = "nil"; line }
  | '}' -> { token_type = RightBrace; lexeme = "}"; literal = "nil"; line }
  | ',' -> { token_type = Comma; lexeme = ","; literal = "nil"; line }
  | '.' -> { token_type = Dot; lexeme = "."; literal = "nil"; line }
  | '-' -> { token_type = Minus; lexeme = "-"; literal = "nil"; line }
  | '+' -> { token_type = Plus; lexeme = "+"; literal = "nil"; line }
  | ';' -> { token_type = Semicolon; lexeme = ";"; literal = "nil"; line }
  | '/' -> { token_type = Slash; lexeme = "/"; literal = "nil"; line }
  | '*' -> { token_type = Star; lexeme = "*"; literal = "nil"; line }
  | _ -> failwith (Printf.sprintf "Cannot convert %c to a token" c)

let token_to_string (tok : t) : string =
  Printf.sprintf "%s %s %s"
    (token_type_to_string tok.token_type)
    tok.lexeme tok.literal

let tokens_to_string (toks : t list) : string =
  toks |> List.map token_to_string |> Astring.String.concat ~sep:"\n"
