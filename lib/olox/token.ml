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
  | (* One or two character tokens. *)
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | (* Literals. *)
    String
  | Number
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
  | Bang -> "Bang"
  | BangEqual -> "BangEqual"
  | Equal -> "Equal"
  | EqualEqual -> "EqualEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | String -> "String"
  | Number -> "Number"
  | Eof -> "Eof"

type t = {
  token_type : token_type;
  lexeme : string;
  literal : string;
  line : int;
}

let eof_token line = { token_type = Eof; lexeme = ""; literal = "nil"; line }

(* [create_string_token] returns a token of type String for the given [str] and [line]. *)
let create_string_token str line =
  let lexeme = "\"" ^ str ^ "\"" in
  { token_type = String; literal = str; lexeme; line }

let create_number_token str line =
  { token_type = Number; literal = str; lexeme = str; line }

let of_string (s : string) (line : int) : t =
  (* Just for Eof as default token type for now *)
  let token = { token_type = Eof; lexeme = s; literal = "nil"; line } in
  match s with
  | "(" -> { token with token_type = LeftParen }
  | ")" -> { token with token_type = RightParen }
  | "{" -> { token with token_type = LeftBrace }
  | "}" -> { token with token_type = RightBrace }
  | "," -> { token with token_type = Comma }
  | "." -> { token with token_type = Dot }
  | "-" -> { token with token_type = Minus }
  | "+" -> { token with token_type = Plus }
  | ";" -> { token with token_type = Semicolon }
  | "/" -> { token with token_type = Slash }
  | "*" -> { token with token_type = Star }
  | "!" -> { token with token_type = Bang }
  | "!=" -> { token with token_type = BangEqual }
  | "=" -> { token with token_type = Equal }
  | "==" -> { token with token_type = EqualEqual }
  | ">" -> { token with token_type = Greater }
  | ">=" -> { token with token_type = GreaterEqual }
  | "<" -> { token with token_type = Less }
  | "<=" -> { token with token_type = LessEqual }
  | _ -> failwith (Printf.sprintf "Cannot convert %s to a token" s)

let token_to_string (tok : t) : string =
  Printf.sprintf "%s %s %s"
    (token_type_to_string tok.token_type)
    tok.lexeme tok.literal

let tokens_to_string (toks : t list) : string =
  toks |> List.map token_to_string |> Astring.String.concat ~sep:"\n"
