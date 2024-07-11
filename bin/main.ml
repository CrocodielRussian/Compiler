let is_digit =
  function '0' .. '9' -> true | _ -> false
let is_alpha = 
  function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_whitespace = 
  function ' ' -> true | _ -> false

let int_option_to_string (i : int option) : string =
  match i with Some x -> string_of_int x | None -> "None"

let invert_number_option x = Option.map (fun n -> -n) x

type oper = Plus | Minus | Multiply | Divide
type expr =
| Number of int
| Unary of oper * expr
| Binary of expr * oper * expr


let text = "                -10         "
let pos = ref 0
let length = String.length text

let skip_whitespaces text pos =
  while !pos<length && is_whitespace text.[!pos] do
    incr pos
  done;;

let positive_number text pos =
  skip_whitespaces text pos;
  let acc = ref "" in
  while !pos < String.length text && is_digit text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos
  done;
  if String.length !acc > 0 
    then Some( Number(int_of_string !acc) ) 
    else None;;


let negate value = 
  match value with
    | Some n -> Some (Unary(Minus, n))
    | None -> None

let rec simplest_expr text pos = 
  skip_whitespaces text pos;
  if !pos >= String.length text 
    then None
    else
      match text.[!pos] with
      | '-' -> incr pos; simplest_expr text pos |> negate 
      | '+' -> incr pos; simplest_expr text pos
      | '(' -> incr pos; None
      | '0'..'9' -> positive_number text pos
      | _ -> None;;


let rec expr_to_string = function
  | Number n -> string_of_int n
  | Unary (op, e) -> 
      let op_str = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"
      in
      op_str ^ "(" ^ expr_to_string e ^ ")"
  | Binary (e1, op, e2) ->
      let op_str = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"
      in
      "(" ^ expr_to_string e1 ^ " " ^ op_str ^ " " ^ expr_to_string e2 ^ ")"


let res = simplest_expr text pos;;
match res with
| Some n -> expr_to_string n |> print_endline
| None -> prerr_endline "None"