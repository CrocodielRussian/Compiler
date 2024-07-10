let () = print_endline "Hello, World!"

type oper = Plus | Multiply | Divide
type expr = Const of int | Binop of oper * expr * expr

let str = "10"
let pos = 0
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let rec number pos =
  if String.get str pos |> is_digit |> not then None else succ pos
