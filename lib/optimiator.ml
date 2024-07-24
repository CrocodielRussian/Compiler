open Parser

let bool_to_int = function true -> 1 | false -> 0

let eval_binary_operation (n1 : int) (op : oper) (n2 : int) =
  match op with
  | Plus -> Number (n1 + n2)
  | Minus -> Number (n1 - n2)
  | Multiply -> Number (n1 * n2)
  | Divide -> Number (n1 / n2)
  | Low -> Number (bool_to_int (n1 < n2))
  | LowOrEqual -> Number (bool_to_int (n1 <= n2))
  | More -> Number (bool_to_int (n1 > n2))
  | MoreOrEqual -> Number (bool_to_int (n1 >= n2))
  | Equal -> Number (bool_to_int (n1 == n2))
  | Unequal -> Number (bool_to_int (n1 != n2))
  | _ -> Binary (Number n1, op, Number n2)

let optimize_left_const_expr (const : int) (op : oper) (subex2 : expr) =
  match subex2 with
  | Number n2 -> eval_binary_operation const op n2
  | _ -> Binary (Number const, op, subex2)

let binary_operation_reverse (subex1 : expr) (op : oper) (subex2 : expr) =
  match op with
  | Plus | Multiply | Equal | Unequal -> Binary (subex2, op, subex1)
  | _ -> Binary (subex1, op, subex2)

let optimize_right_expr (subex1 : expr) (op : oper) (subex2 : expr) =
  match subex2 with
  | Number _ -> binary_operation_reverse subex1 op subex2
  | _ -> Binary (subex1, op, subex2)

let rec optimize_unary_expr (op : oper) (subex : expr) =
  let optisubex = optimize_expr subex in
  match op with
  | Plus -> optisubex
  | Minus -> (
      match optisubex with
      | Number n -> Number (-n)
      | _ -> Unary (Minus, optisubex))
  | _ ->
      failwith
        ("ASTError: unexpected unary operator: "
        ^ string_of_binary_operator op
        ^ ".")

and optimize_binary_expr (subex1 : expr) (op : oper) (subex2 : expr) =
  let optisubex1 = optimize_expr subex1 in
  let optisubex2 = optimize_expr subex2 in
  match optisubex1 with
  | Number n1 -> optimize_left_const_expr n1 op optisubex2
  | _ -> optimize_right_expr optisubex1 op optisubex2

and optimize_expr (ex : expr) =
  match ex with
  | Variable _ -> ex
  | Number _ -> ex
  | Unary (op, subex) -> optimize_unary_expr op subex
  | AssignExpression (v, op, subex) ->
      AssignExpression (v, op, optimize_expr subex)
  | Binary (subex1, op, subex2) -> optimize_binary_expr subex1 op subex2
  | _ -> ex

let optimize_stmt (stmt : statement) =
  match stmt with
  | Expression ex -> Expression (optimize_expr ex)
  | AssignStatement (v, ex) -> AssignStatement (v, optimize_expr ex)
  | _ -> stmt

let optimize_ast (ast : statement list) : statement list =
  print_endline (string_of_statements "" 0 ast);
  let new_ast = ref [] in
  List.iter (fun stmt -> new_ast := !new_ast @ [ optimize_stmt stmt ]) ast;
  print_endline (string_of_statements "" 0 !new_ast);
  !new_ast
