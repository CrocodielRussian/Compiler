open Option
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' -> true | _ -> false

let text = "   10000"
let int_option_to_string (i : int option) : string =
  match i with Some x -> string_of_int x | None -> "None"
type oper = Plus | Multiply | Divide | Minus | Invalid
type expr =
| Number of int
| Unary of oper * expr
| Binary of expr * oper * expr
| Error


let pos = ref 0
let length = String.length text

let ws () =
  while !pos<length && is_whitespace text.[!pos] do
    incr pos
  done;;

let number =
  ws();
  let acc = ref "" in
  while !pos < length && is_digit text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos
  done;
  if String.length !acc > 0 
  then Number(int_of_string !acc)
  else Error;;


let num = Number 10;;

let op = function
  | '+' -> Plus
  | '-' -> Minus
  | '*' -> Multiply
  | '/' -> Divide
  | _ -> Invalid

let rec parse_expr text =
  ws();
  let head = number in 
  let operation = op text.[!pos] in

  if operation != Invalid
    then Binary(head, operation, parse_expr text)
  else
    head
