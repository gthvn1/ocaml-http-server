(*
    expression     → literal
                   | unary
                   | binary
                   | grouping ;
    literal        → NUMBER | STRING | "true" | "false" | "nil" ;
    grouping       → "(" expression ")" ;
    unary          → ( "-" | "!" ) expression ;
    binary         → expression operator expression ;
    operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
                   | "+"  | "-"  | "*" | "/" ;
*)

type t = Expression of expression

and expression =
  | Literal of literal
  | Grouping of expression
  | Unary of unary_op * expression
  | Binary of binary_op * expression * expression

and unary_op = Negate | Bang

and binary_op =
  | Star
  | Slash
  | Minus
  | Plus
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | BangEqual
  | EqualEqual

and literal = Number of string | String of string | True | False | Nil

(* Constructors helpers *)
(* Literal *)
let string_literal s = Literal (String s)
let number_literal s = Literal (Number s)
let true_literal () = Literal True
let false_literal () = Literal False
let nil_literal () = Literal Nil

(* Grouping *)
let group_expr expr = Grouping expr

(* Unary *)
let negate_unary expr = Unary (Negate, expr)
let bang_unary expr = Unary (Bang, expr)

(* Binary *)
let star_binary left right = Binary (Star, left, right)
let slash_binary left right = Binary (Slash, left, right)
let minus_binary left right = Binary (Minus, left, right)
let plus_binary left right = Binary (Plus, left, right)
let greater_binary left right = Binary (Greater, left, right)
let greater_equal_binary left right = Binary (GreaterEqual, left, right)
let less_binary left right = Binary (Less, left, right)
let less_equal_binary left right = Binary (LessEqual, left, right)
let bang_equal_binary left right = Binary (BangEqual, left, right)
let equal_equal_binary left right = Binary (EqualEqual, left, right)

let rec to_string (ast : t) : string =
  match ast with
  | Expression (Literal (String s)) -> s
  | Expression (Literal (Number n)) -> n
  | Expression (Literal True) -> "true"
  | Expression (Literal False) -> "false"
  | Expression (Literal Nil) -> "nil"
  | Expression (Grouping expr) -> "(group " ^ to_string (Expression expr) ^ ")"
  | Expression (Unary (op, expr)) ->
      unary_to_string op ^ to_string (Expression expr) ^ ")"
  | Expression (Binary (op, left, right)) ->
      binary_to_string op
      ^ to_string (Expression left)
      ^ " "
      ^ to_string (Expression right)
      ^ ")"

and unary_to_string = function Negate -> "(- " | Bang -> "(! "

and binary_to_string = function
  | Star -> "(* "
  | Slash -> "(/ "
  | Minus -> "(- "
  | Plus -> "(+ "
  | Greater -> "(> "
  | GreaterEqual -> "(>= "
  | Less -> "(< "
  | LessEqual -> "(<= "
  | BangEqual -> "(!= "
  | EqualEqual -> "(== "
