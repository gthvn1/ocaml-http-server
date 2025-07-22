module A = Ast

(*

Here is the logic of the parser:
  expression     → equality ;
  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  term           → factor ( ( "-" | "+" ) factor )* ;
  factor         → unary ( ( "/" | "*" ) unary )* ;
  unary          → ( "!" | "-" ) unary
                 | primary ;
  primary        → NUMBER | STRING | "true" | "false" | "nil"
                 | "(" expression ")" ;
*)

type parse_result = (A.expression * Token.t list, string) result
type token_list = Token.t list

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let rec expression (toks : token_list) : parse_result = equality toks

and equality (toks : token_list) : parse_result =
  let* left, rest = comparison toks in
  equality_loop left rest

and equality_loop (left : A.expression) (toks : token_list) : parse_result =
  match toks with
  | { token_type = BangEqual; _ } :: rest ->
      let* right, r = comparison rest in
      let node = A.bang_equal_binary left right in
      equality_loop node r
  | { token_type = EqualEqual; _ } :: rest ->
      let* right, r = comparison rest in
      let node = A.equal_equal_binary left right in
      equality_loop node r
  | _ -> Ok (left, toks)

and comparison (toks : token_list) : parse_result =
  let* left, rest = term toks in
  comparison_loop left rest

and comparison_loop (left : A.expression) (toks : token_list) : parse_result =
  match toks with
  | { token_type = Greater; _ } :: rest ->
      let* right, r = term rest in
      let node = A.greater_binary left right in
      comparison_loop node r
  | { token_type = GreaterEqual; _ } :: rest ->
      let* right, r = term rest in
      let node = A.greater_equal_binary left right in
      comparison_loop node r
  | { token_type = Less; _ } :: rest ->
      let* right, r = term rest in
      let node = A.less_binary left right in
      comparison_loop node r
  | { token_type = LessEqual; _ } :: rest ->
      let* right, r = term rest in
      let node = A.less_equal_binary left right in
      comparison_loop node r
  | _ -> Ok (left, toks)

and term (toks : token_list) : parse_result =
  let* left, rest = factor toks in
  term_loop left rest

and term_loop (left : A.expression) (toks : token_list) : parse_result =
  match toks with
  | { token_type = Minus; _ } :: rest ->
      let* right, r = factor rest in
      let node = A.minus_binary left right in
      term_loop node r
  | { token_type = Plus; _ } :: rest ->
      let* right, r = factor rest in
      let node = A.plus_binary left right in
      term_loop node r
  | _ -> Ok (left, toks)

and factor (toks : token_list) : parse_result =
  let* left, rest = unary toks in
  factor_loop left rest

and factor_loop (left : A.expression) (toks : token_list) : parse_result =
  match toks with
  | { token_type = Slash; _ } :: rest ->
      let* right, r = unary rest in
      let node = A.slash_binary left right in
      factor_loop node r
  | { token_type = Star; _ } :: rest ->
      let* right, r = unary rest in
      let node = A.star_binary left right in
      factor_loop node r
  | _ -> Ok (left, toks)

and unary (toks : token_list) : parse_result =
  match toks with
  | { token_type = Bang; _ } :: rest ->
      let* right, r = unary rest in
      Ok (A.bang_unary right, r)
  | { token_type = Minus; _ } :: rest ->
      let* right, r = unary rest in
      Ok (A.negate_unary right, r)
  | _ -> primary toks

and primary (toks : token_list) : parse_result =
  match toks with
  | { token_type = False; _ } :: r -> Ok (A.false_literal (), r)
  | { token_type = True; _ } :: r -> Ok (A.true_literal (), r)
  | { token_type = Nil; _ } :: r -> Ok (A.nil_literal (), r)
  | { token_type = String; literal; _ } :: r -> Ok (A.string_literal literal, r)
  | { token_type = Number; literal; _ } :: r -> Ok (A.number_literal literal, r)
  | { token_type = LeftParen; _ } :: r -> (
      let* expr, rest = expression r in
      match rest with
      | { token_type = RightParen; _ } :: r' -> Ok (A.group_expr expr, r')
      | token :: _ ->
          Error
            (Printf.sprintf "[line %d] Error at '%s': Expect expression."
               token.line token.literal)
      | [] -> Error "right parens not found")
  | _ -> Error "syntax error"

(* [parse] transforms a list of tokens into an AST *)
let parse (toks : token_list) : (A.t, string) result =
  match expression toks with
  | Error s -> Error s
  | Ok (ast, _) -> Ok (Expression ast)
