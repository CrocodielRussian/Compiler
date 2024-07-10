let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let int_option_to_string (i : int option) : string =
  match i with
  | Some x -> string_of_int x
  | None -> "None"

let text = "     10";;

let parse_int (s : string) : int option =
  let rec parse_int' (i : int) (s : string) (idx : int) : int option =
    if idx >= String.length s then
      Some i
    else
      match s.[idx] with
      | '0' .. '9' as c -> parse_int' (i * 10 + (Char.code c - Char.code '0')) s (idx + 1)
      | _ -> None
  in
  parse_int' 0 s 0;;

parse_int text |> int_option_to_string |> print_endline;;


(* type oper = Plus | Multiply | Divide
type expr =
| Const of int
| Binop of oper * expr * expr *)

