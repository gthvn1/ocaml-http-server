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
    Identifier
  | String
  | Number
  | (* Keywords. *)
    And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Eof

let token_type_to_string = function
  | LeftParen -> "LEFT_PAREN"
  | RightParen -> "RIGHT_PAREN"
  | LeftBrace -> "LEFT_BRACE"
  | RightBrace -> "RIGHT_BRACE"
  | Comma -> "COMMA"
  | Dot -> "DOT"
  | Minus -> "MINUS"
  | Plus -> "PLUS"
  | Semicolon -> "SEMICOLON"
  | Slash -> "SLASH"
  | Star -> "STAR"
  | Bang -> "BANG"
  | BangEqual -> "BANG_EQUAL"
  | Equal -> "EQUAL"
  | EqualEqual -> "EQUAL_EQUAL"
  | Greater -> "GREATER"
  | GreaterEqual -> "GREATER_EQUAL"
  | Less -> "LESS"
  | LessEqual -> "LESS_EQUAL"
  | Identifier -> "IDENTIFIER"
  | String -> "STRING"
  | Number -> "NUMBER"
  | And -> "AND"
  | Class -> "CLASS"
  | Else -> "ELSE"
  | False -> "FALSE"
  | Fun -> "FUN"
  | For -> "FOR"
  | If -> "IF"
  | Nil -> "NIL"
  | Or -> "OR"
  | Print -> "PRINT"
  | Return -> "RETURN"
  | Super -> "SUPER"
  | This -> "THIS"
  | True -> "TRUE"
  | Var -> "VAR"
  | While -> "WHILE"
  | Eof -> "EOF"

let to_keywords = function
  | "and" -> And
  | "class" -> Class
  | "else" -> Else
  | "false" -> False
  | "fun" -> Fun
  | "for" -> For
  | "if" -> If
  | "nil" -> Nil
  | "or" -> Or
  | "print" -> Print
  | "return" -> Return
  | "super" -> Super
  | "this" -> This
  | "true" -> True
  | "var" -> Var
  | "while" -> While
  | _ -> Identifier

type t = {
  token_type : token_type;
  lexeme : string;
  literal : string;
  line : int;
}

let eof_token line = { token_type = Eof; lexeme = ""; literal = "null"; line }

(* [create_string_token] returns a token of type String for the given [str] and [line]. *)
let create_string_token str line =
  let lexeme = "\"" ^ str ^ "\"" in
  { token_type = String; literal = str; lexeme; line }

(* We are expecting at least one '.' otherwise it raise an error *)
let normalize_decimal (str : string) : string =
  if Astring.String.exists (fun c -> c = '.') str then
    let parts = String.split_on_char '.' str in
    match parts with
    | [ int_part; frac_part ] ->
        let frac_part' =
          let rec trim s =
            if String.ends_with ~suffix:"0" s then
              trim (String.sub s 0 (String.length s - 1))
            else s
          in
          trim frac_part
        in
        if frac_part' = "" then int_part ^ ".0" else int_part ^ "." ^ frac_part'
    | _ -> failwith ("Error: We don't expect several dot in the decimal: " ^ str)
  else str ^ ".0"

let create_number_token str line =
  (* literal value for an integer is represented as "42.0" (with a decimal point).
     We also need to remove extra 0s at the end:
     - 42.000 -> 42.0
     - 42.340 -> 42.34
     - 42.0   -> 42.0
     - 42     -> 42.0
      *)
  let literal = normalize_decimal str in
  { token_type = Number; literal; lexeme = str; line }

let create_keyword_token keyword line =
  { token_type = to_keywords keyword; literal = "null"; lexeme = keyword; line }

let of_string (s : string) (line : int) : t =
  (* Just for Eof as default token type for now *)
  let token = { token_type = Eof; lexeme = s; literal = "null"; line } in
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
