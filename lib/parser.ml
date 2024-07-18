type oper =
  | Plus
  | Multiply
  | Divide
  | Minus
  | Low
  | More
  | LowOrEqual
  | MoreOrEqual
  | Equal
  | Unequal
  | Invalid

(* Assotiate with: := | += | *= | /= | -= | Invalid *)
type assign_oper =
  | DefaultAssign
  | PlusAssign
  | MultiplyAssign
  | DivideAssign
  | MinusAssign
  | InvalidAssing

type expr =
  | Variable of string
  | Number of int (* 12 *)
  | Unary of oper * expr (* -12 *)
  | Binary of expr * oper * expr (* 14  + 12 *)
  | AssignExpression of string * assign_oper * expr
(* a := 12*)
(* [@@derive show] *)

type statement =
  | Expression of expr
  | AssignStatement of string * expr
  | Empty
  | While of expr * statement list
  | If of expr * statement list * statement list
(* [@@derive show] *)

let string_of_unary_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Invalid -> failwith "AST Error: unexpected operator."
  | _ -> failwith "AST Error: unexpected unary operator."

let string_of_binary_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Low -> "<"
  | LowOrEqual -> "<="
  | More -> ">"
  | MoreOrEqual -> ">="
  | Equal -> "=="
  | Unequal -> "!="
  | Invalid -> failwith "AST Error: unexpected unary operator."

let string_of_assign_operator = function
  | DefaultAssign -> ":="
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | MultiplyAssign -> "*="
  | DivideAssign -> "/="
  | InvalidAssing -> "?=?"

let rec string_of_expression text pos = function
  | Variable n -> n
  | Number n -> string_of_int n
  | Unary (op, e) ->
      string_of_unary_operator op ^ "(" ^ string_of_expression text pos e ^ ")"
  | Binary (e1, op, e2) ->
      "("
      ^ string_of_expression text pos e1
      ^ " "
      ^ string_of_binary_operator op
      ^ " "
      ^ string_of_expression text pos e2
      ^ ")"
  | AssignExpression (v, op, e) ->
      "(" ^ v ^ " "
      ^ string_of_assign_operator op
      ^ " "
      ^ string_of_expression text pos e
      ^ ")"

let rec string_of_statements text pos stmts =
  let str_lst =
    List.map (fun stmt -> string_of_statement text pos stmt) stmts
  in
  String.concat "\n" str_lst

and string_of_statement text pos = function
  | Empty -> ""
  | Expression e -> string_of_expression text pos e ^ ";"
  | AssignStatement (v, e2) ->
      "var " ^ v ^ " := " ^ string_of_expression text pos e2 ^ ";"
  | While (e1, stmts) ->
      "while "
      ^ string_of_expression text pos e1
      ^ " do\n"
      ^ string_of_statements text pos stmts
      ^ "\ndone"
  | If (e1, if_stmts, else_stmts) ->
      if else_stmts = [ Empty ] then
        "if "
        ^ string_of_expression text pos e1
        ^ " then\n"
        ^ string_of_statements text pos if_stmts
        ^ "\nendif"
      else
        "if "
        ^ string_of_expression text pos e1
        ^ " then\n"
        ^ string_of_statements text pos if_stmts
        ^ "\nelse\n"
        ^ string_of_statements text pos else_stmts
        ^ "\nendif"

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' | '\r' | '\t' | '\n' -> true | _ -> false

let skip_whitespaces text pos =
  let length = String.length text in
  while !pos < length && is_whitespace text.[!pos] do
    incr pos
  done

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

let parse_added_operation = function '+' -> Plus | '-' -> Minus | _ -> Invalid

let parse_mult_operation = function
  | '*' -> Multiply
  | '/' -> Divide
  | _ -> Invalid

let parse_assgn_operation text pos =
  skip_whitespaces text pos;
  let len = String.length text in
  let check_substring sub =
    let sub_len = String.length sub in
    !pos + sub_len <= len && String.sub text !pos sub_len = sub
  in
  let eval_op_assing = function
    | ":=" -> DefaultAssign
    | "+=" -> PlusAssign
    | "-=" -> MinusAssign
    | "*=" -> MultiplyAssign
    | "/=" -> DivideAssign
    | _ -> InvalidAssing
  in
  if
    check_substring ":=" || check_substring "+=" || check_substring "-="
    || check_substring "*=" || check_substring "/="
  then (
    pos := !pos + 2;
    eval_op_assing (String.sub text (!pos - 2) 2))
  else InvalidAssing

let parse_compare_operation text pos =
  skip_whitespaces text pos;
  let len = String.length text in
  let check_substring sub =
    let sub_len = String.length sub in
    !pos + sub_len <= len && String.sub text !pos sub_len = sub
  in
  let eval_op_compare = function
    | ">=" -> MoreOrEqual
    | "<=" -> LowOrEqual
    | "==" -> Equal
    | "!=" -> Unequal
    | ">" -> More
    | "<" -> Low
    | _ -> Invalid
  in
  if
    check_substring ">=" || check_substring "<=" || check_substring "=="
    || check_substring "!="
  then (
    pos := !pos + 2;
    eval_op_compare (String.sub text (!pos - 2) 2))
  else if check_substring ">" || check_substring "<" then (
    incr pos;
    eval_op_compare (String.sub text (!pos - 1) 1))
  else Invalid

let rec parse_expr text pos =
  skip_whitespaces text pos;
  let head = parse_math_expr text pos in
  skip_whitespaces text pos;
  if !pos >= String.length text then head
  else
    match head with
    | Variable name -> parse_assign_expr name text pos
    | _ -> parse_compare_expr head text pos

and parse_compare_expr head text pos =
  skip_whitespaces text pos;
  let operation = parse_compare_operation text pos in
  if operation != Invalid then Binary (head, operation, parse_expr text pos)
  else head

and parse_assign_expr name text pos =
  skip_whitespaces text pos;
  let operation = parse_assgn_operation text pos in
  if operation = InvalidAssing then parse_compare_expr (Variable name) text pos
  else AssignExpression (name, operation, parse_expr text pos)

and parse_math_expr text pos =
  skip_whitespaces text pos;
  let head = parse_mult_expr text pos in
  skip_whitespaces text pos;
  if !pos >= String.length text then head
  else
    let operation = parse_added_operation text.[!pos] in
    if !pos + 1 < String.length text && text.[!pos + 1] = '=' then head
    else if operation != Invalid then (
      incr pos;
      Binary (head, operation, parse_math_expr text pos))
    else head

and parse_mult_expr text pos =
  skip_whitespaces text pos;
  let head = parse_simplest_expr text pos in
  skip_whitespaces text pos;
  if !pos >= String.length text then head
  else
    let operation = parse_mult_operation text.[!pos] in
    if !pos + 1 < String.length text && text.[!pos + 1] = '=' then head
    else if operation != Invalid then (
      incr pos;
      Binary (head, operation, parse_mult_expr text pos))
    else head

and parse_simplest_expr text pos =
  skip_whitespaces text pos;
  if !pos >= String.length text then
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find expression.")
  else
    match text.[!pos] with
    | '-' ->
        incr pos;
        Unary (Minus, parse_simplest_expr text pos)
    | '+' ->
        incr pos;
        parse_simplest_expr text pos
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
         ^ " unexpected symbol: '"
          ^ Char.escaped text.[!pos]
          ^ "'.")

let check_exists_simple_stmt_close text pos =
  skip_whitespaces text pos;
  if !pos < String.length text then
    match text.[!pos] with
    | ';' ->
        incr pos;
        true
    | _ -> false
  else false

let parse_expr_statement text pos =
  skip_whitespaces text pos;
  let result = parse_expr text pos in
  if check_exists_simple_stmt_close text pos then Expression result
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find close symbol of expression statement: ';'.")

let assert_assign_statement_op text pos =
  skip_whitespaces text pos;
  match String.sub text !pos 2 with
  | ":=" -> pos := !pos + 2
  | _ ->
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find assign operator. Find " ^ String.sub text !pos 2 ^ "."
        )

let parser_assign_statement text pos =
  skip_whitespaces text pos;
  let ident = identifier text pos in
  assert_assign_statement_op text pos;
  let assign = AssignStatement (ident, parse_expr text pos) in
  if check_exists_simple_stmt_close text pos then assign
  else
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find close symbol of statement: ';'.")

let check_do_exists text pos =
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

let check_done_exists text pos =
  skip_whitespaces text pos;
  if !pos + 4 < String.length text then
    match String.sub text !pos 4 with "done" -> false | _ -> true
  else false

let check_then_exists text pos =
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

let check_else_and_endif_construction_exists text pos =
  skip_whitespaces text pos;
  if !pos + 5 < String.length text then
    match String.sub text !pos 5 with
    | "else " | "else\t" | "else\n" | "else\r" -> false
    | "endif" -> false
    | _ -> true
  else false

let rec parse_while_loop_statement text pos =
  skip_whitespaces text pos;
  if !pos > String.length text then
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find bool expression in while.")
  else
    let expression = parse_expr text pos in
    if check_do_exists text pos then (
      let loop =
        While (expression, parse_statements text pos check_done_exists)
      in
      skip_whitespaces text pos;
      if !pos + 4 <= String.length text then
        match String.sub text !pos 4 with
        | "done" ->
            pos := !pos + 4;
            loop
        | _ ->
            failwith
              ("Parser Error: on position " ^ (!pos |> string_of_int)
             ^ " couldn't find assign statement.")
      else
        failwith
          ("Parser Error: on position " ^ (!pos |> string_of_int)
         ^ " couldn't find done."))
    else
      failwith
        ("Parser Error: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find do.")

and parse_if_statement text pos =
  if !pos > String.length text then
    failwith
      ("Parser Error: on position " ^ (!pos |> string_of_int)
     ^ " couldn't find bool expression in if.")
  else
    let expression = parse_expr text pos in
    if check_then_exists text pos then (
      skip_whitespaces text pos;
      let if_fork =
        parse_statements text pos check_else_and_endif_construction_exists
      in
      skip_whitespaces text pos;
      if !pos + 5 <= String.length text then
        match String.sub text !pos 5 with
        | "endif" ->
            pos := !pos + 5;
            If (expression, if_fork, [ Empty ])
        | "else " | "else\t" | "else\n" | "else\r" ->
            pos := !pos + 5;
            If
              ( expression,
                if_fork,
                parse_statements text pos
                  check_else_and_endif_construction_exists )
        | _ ->
            failwith
              ("Parser Error: on position " ^ (!pos |> string_of_int)
             ^ " couldn't find assign statement.")
      else (
        print_int (String.length text);
        failwith
          ("ParserError: on position " ^ (!pos |> string_of_int)
         ^ " couldn't find else.")))
    else
      failwith
        ("ParserError: on position " ^ (!pos |> string_of_int)
       ^ " couldn't find then.")

and parse_statements text pos check =
  let all = ref [] in
  while check text pos do
    skip_whitespaces text pos;
    let ident = identifier text pos in
    match ident with
    | "while" ->
        let result = parse_while_loop_statement text pos in
        all := !all @ [ result ]
    | "if" ->
        let result = parse_if_statement text pos in
        all := !all @ [ result ]
    | "var" ->
        let result = parser_assign_statement text pos in
        all := !all @ [ result ]
    | _ ->
        pos := !pos - String.length ident;
        let result = parse_expr_statement text pos in
        all := !all @ [ result ]
  done;
  !all

let check_program_end text pos = !pos < String.length text
let parse_program text pos = parse_statements text pos check_program_end
