let rot13_char c =
  let open Char in
  match c with
  | 'a' .. 'z' -> chr (((code c - code 'a' + 13) mod 26) + code 'a')
  | 'A' .. 'Z' -> chr (((code c - code 'A' + 13) mod 26) + code 'A')
  | _ -> c

let encode = String.map rot13_char
