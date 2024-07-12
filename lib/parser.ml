open Option

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' -> true | _ -> false

let int_option_to_string (i : int option) : string =
  match i with Some x -> string_of_int x | None -> "None"

let skip_whitespaces text pos =
  let length = String.length text in
  while !pos < length && is_whitespace text.[!pos] do
    incr pos
  done

type oper = Plus | Multiply | Divide | Minus | Invalid

type expr =
  | Variable of string
  | Number of int
  | Unary of oper * expr
  | Binary of expr * oper * expr

let positive_number text pos =
  skip_whitespaces text pos;
  let acc = ref "" in
  let length = String.length text in
  while !pos < length && is_digit text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos
  done;
  if String.length !acc > 0 then Number (int_of_string !acc)
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find numbers.")

let op_add = function '+' -> Plus | '-' -> Minus | _ -> Invalid
let op_mult = function '*' -> Multiply | '/' -> Divide | _ -> Invalid

let rec parse_expr_mul text pos =
  skip_whitespaces text pos;
  let head = simplest_expr text pos in
  skip_whitespaces text pos;
  if !pos >= String.length text then head
  else
    let operation = op_mult text.[!pos] in
    if operation != Invalid then (
      incr pos;
      Binary (head, operation, parse_expr_mul text pos))
    else head

and parse_expr text pos =
  skip_whitespaces text pos;
  let head = parse_expr_mul text pos in
  skip_whitespaces text pos;
  if !pos >= String.length text then head
  else
    let operation = op_add text.[!pos] in
    if operation != Invalid then (
      incr pos;
      Binary (head, operation, parse_expr text pos))
    else head

and simplest_expr text pos =
  skip_whitespaces text pos;
  if !pos >= String.length text then
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find expression.")
  else
    match text.[!pos] with
    | '-' ->
        incr pos;
        Unary (Minus, simplest_expr text pos)
    | '+' ->
        incr pos;
        simplest_expr text pos
    | '(' ->
        incr pos;
        let result_of_searching = parse_expr text pos in
        skip_whitespaces text pos;
        if !pos < String.length text && text.[!pos] = ')' then (
          incr pos;
          result_of_searching)
        else
          failwith
            ("Parse Error: on position " ^ (!pos |> string_of_int)
           ^ " couldn't find symbol of closed bracket: ')'.")
    | '0' .. '9' -> positive_number text pos
    | _ ->
        failwith
          ("Parse Error: on position " ^ (!pos |> string_of_int)
         ^ " unexpected symbol.")

let rec expr_to_string text pos = function
  | Variable n -> "<var: " ^ n ^ ">"
  | Number n -> string_of_int n
  | Unary (op, e) ->
      let op_str =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Invalid ->
            failwith
              ("Parse Error: on position " ^ (!pos |> string_of_int)
             ^ " unexpected operator.")
      in
      op_str ^ "(" ^ expr_to_string text pos e ^ ")"
  | Binary (e1, op, e2) ->
      let op_str =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Invalid ->
            failwith
              ("Parse Error: on position " ^ (!pos |> string_of_int)
             ^ " unexpected operator.")
      in
      "(" ^ expr_to_string text pos e1 ^ " " ^ op_str ^ " "
      ^ expr_to_string text pos e2 ^ ")"

(* Assotiate with: := | += | *= | /= | -= | Invalid *)
type asign_oper =
  | DefaultAssign
  | PlusAssign
  | MultiplyAssign
  | DivideAssign
  | MinusAssign

type statement = Assign of string * asign_oper * expr | Empty
type program = Program of statement list

let identifier text pos =
  let acc = ref "" in
  while !pos < String.length text && is_alpha text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos
  done;
  !acc

let stmt_op text pos =
  match String.sub text !pos 2 with
  | ":=" -> DefaultAssign
  | "+=" -> PlusAssign
  | "-=" -> MinusAssign
  | "*=" -> MultiplyAssign
  | "/=" -> DivideAssign
  | _ ->
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find asign operator.")

let variable text pos =
  skip_whitespaces text pos;
  let acc = identifier text pos in
  if String.length acc > 0 then Variable acc
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find variable.")

let stmt_to_string text pos = function
  | Empty -> ""
  | Assign (e1, op, e2) ->
      let op_str =
        match op with
        | PlusAssign -> "+="
        | MinusAssign -> "-="
        | MultiplyAssign -> "*="
        | DivideAssign -> "/="
        | DefaultAssign -> ":="
      in
      "<var: " ^ e1 ^ "> " ^ op_str ^ " " ^ expr_to_string text pos e2 ^ ";"

let stmt text pos =
  skip_whitespaces text pos;
  if !pos >= String.length text then Empty
  else
    let ident = identifier text pos in
    skip_whitespaces text pos;
    let operation = stmt_op text pos in
    pos := !pos + 2;
    let asign = Assign (ident, operation, parse_expr text pos) in
    stmt_to_string text pos asign |> prerr_endline;
    skip_whitespaces text pos;
    if !pos < String.length text then
      match text.[!pos] with
      | ';' -> asign
      | _ ->
          failwith
            ("Parser Error: on position " ^ (!pos |> string_of_int)
           ^ " couldn't find asign statement.")
    else
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find close symbol of statement: ';'.")
