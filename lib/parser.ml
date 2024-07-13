let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' | '\r' | '\t' | '\n' -> true | _ -> false

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

let identifier text pos =
  let acc = ref "" in
  while !pos < String.length text && is_alpha text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos
  done;
  !acc

let variable text pos =
  skip_whitespaces text pos;
  let acc = identifier text pos in
  if String.length acc > 0 then Variable acc
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find variable.")

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
    | 'a' .. 'z' | 'A' .. 'Z' -> variable text pos
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

type statement =
  | Assign of string * asign_oper * expr
  | Empty
  | While of expr * statement list
  | If of expr * statement list * statement list

type program = Program of statement list

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
       ^ " couldn't find asign operator. Find " ^ String.sub text !pos 2 ^ ".")

let rec stmt_to_string text pos = function
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
  | While (e1, stmts) ->
      "while " ^ expr_to_string text pos e1 ^ " do\n"
      ^ stmts_to_string text pos stmts
      ^ "\ndone"
  | If (e1, if_stmts, else_stmts) ->
      "If " ^ expr_to_string text pos e1 ^ " then\n"
      ^ stmts_to_string text pos if_stmts
      ^ "\nelse\n"
      ^ stmts_to_string text pos else_stmts
      ^ "\nendif"

and stmts_to_string text pos stmts =
  let str_lst = List.map (fun stmt -> stmt_to_string text pos stmt) stmts in
  String.concat "\n" (List.rev str_lst)

let asign_stmt ident text pos =
  skip_whitespaces text pos;
  if !pos >= String.length text then Empty
  else
    let operation = stmt_op text pos in
    pos := !pos + 2;
    let asign = Assign (ident, operation, parse_expr text pos) in
    skip_whitespaces text pos;
    if !pos < String.length text then
      match text.[!pos] with
      | ';' ->
          incr pos;
          asign
      | _ ->
          failwith
            ("Parser Error: on position " ^ (!pos |> string_of_int)
           ^ " couldn't find asign statement for " ^ ident ^ ".")
    else
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find close symbol of statement: ';'.")

let check_do text pos =
  skip_whitespaces text pos;
  if !pos + 2 < String.length text then
    match String.sub text !pos 2 with
    | "do" ->
        pos := !pos + 2;
        true
    | _ -> false
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find do.")

let check_done text pos =
  skip_whitespaces text pos;
  if !pos + 4 < String.length text then
    match String.sub text !pos 4 with "done" -> false | _ -> true
  else false

let check_then text pos =
  skip_whitespaces text pos;
  if !pos + 4 < String.length text then
    match String.sub text !pos 4 with
    | "then" ->
        pos := !pos + 4;
        true
    | _ -> false
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find then.")

let check_break_condition text pos =
  skip_whitespaces text pos;
  if !pos + 5 < String.length text then
    match String.sub text !pos 5 with
    | "else " | "else\t" | "else\n" | "else\r" -> false
    | "endif" -> false
    | _ -> true
  else false

let global_end text pos = !pos < String.length text

let rec while_loop_statement text pos =
  skip_whitespaces text pos;
  if !pos > String.length text then
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find bool expression in while.")
  else
    let expression = parse_expr text pos in
    if check_do text pos then (
      let loop = While (expression, statements text pos check_done) in
      skip_whitespaces text pos;
      if !pos + 4 <= String.length text then
        match String.sub text !pos 4 with
        | "done" ->
            pos := !pos + 4;
            loop
        | _ ->
            failwith
              ("Parser Error: on position " ^ (!pos |> string_of_int)
             ^ " couldn't find asign statement.")
      else
        failwith
          ("Parser Error: on position " ^ (!pos |> string_of_int)
         ^ " couldn't find done."))
    else
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find do.")

and if_statement text pos =
  if !pos > String.length text then
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find bool expression in if.")
  else
    let expression = parse_expr text pos in
    if check_then text pos then (
      skip_whitespaces text pos;
      let if_fork = statements text pos check_break_condition in
      skip_whitespaces text pos;
      if !pos + 5 <= String.length text then
        match String.sub text !pos 5 with
        | "endif" ->
            pos := !pos + 5;
            If (expression, if_fork, [ Empty ])
        | "else " | "else\t" | "else\n" | "else\r" ->
            pos := !pos + 5;
            If (expression, if_fork, statements text pos check_break_condition)
        | _ ->
            failwith
              ("Parser Error: on position " ^ (!pos |> string_of_int)
             ^ " couldn't find asign statement.")
      else (
        print_int (String.length text);
        failwith
          ("Parser Error: on position " ^ (!pos |> string_of_int)
         ^ " couldn't find else.")))
    else
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find then.")

and statements text pos check =
  let all = ref [] in
  while check text pos do
    skip_whitespaces text pos;
    let ident = identifier text pos in
    match ident with
    | "while" ->
        let result = while_loop_statement text pos in
        all := result :: !all
    | "if" ->
        let result = if_statement text pos in
        all := result :: !all
    | _ ->
        let result = asign_stmt ident text pos in
        all := result :: !all
  done;
  !all

let global_statements text pos = statements text pos global_end
