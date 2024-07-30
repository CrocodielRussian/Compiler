open Parser
open Exceptions
let bool_to_int = function true -> 1 | false -> 0
let bool_of_int num = num != 0

let eval_binary_operation (n1 : int) (op : oper) (n2 : int) =
  match op with
  | Plus -> n1 + n2
  | Minus -> n1 - n2
  | Multiply -> n1 * n2
  | Divide -> (
      match n2 with 0 -> throw_except(LogicErrorMath("divide by zero")) | _ -> n1 / n2) 
  | Low -> bool_to_int (n1 < n2)
  | LowOrEqual -> bool_to_int (n1 <= n2)
  | More -> bool_to_int (n1 > n2)
  | MoreOrEqual -> bool_to_int (n1 >= n2)
  | Equal -> bool_to_int (n1 == n2)
  | Unequal -> bool_to_int (n1 != n2)
  | AndOper -> bool_to_int (bool_of_int n1 || bool_of_int n2)
  | OrOper -> bool_to_int (bool_of_int n1 || bool_of_int n2)
  | Invalid -> throw_except(ASTError("unexpected operator"))

let valid_op_const (op : oper) =
  match op with Plus -> 0 | Multiply -> 1 | _ -> 0

let constants_compression (op : oper) (expressions : expr list) : expr list =
  let compression_expressions = ref [] in
  let const = ref (valid_op_const op) in
  let do_comprassion = ref false in
  List.iter
    (fun ex ->
      match ex with
      | Number n ->
          const := eval_binary_operation !const op n;
          do_comprassion := true
      | _ -> compression_expressions := !compression_expressions @ [ ex ])
    expressions;
  if !do_comprassion then
    compression_expressions := !compression_expressions @ [ Number !const ];
  !compression_expressions

let pop_front = function [] -> [] | _ :: tl -> tl

let rec deep_optimize_binary_expr (ex1 : expr) (op : oper) (ex2 : expr) : expr =
  match op with
  | Plus | Multiply ->
      let operands = ref [] in
      let queue = ref ([ ex1 ] @ [ ex2 ]) in
      while List.length !queue > 0 do
        let ex = List.nth !queue 0 in
        queue := pop_front !queue;
        match ex with
        | Binary (subex1, subop, subex2) ->
            if op = subop then queue := !queue @ [ subex1 ] @ [ subex2 ]
            else operands := !operands @ [ ex ]
        | _ -> operands := !operands @ [ ex ]
      done;
      operands := constants_compression op !operands;
      let result_expr = ref (List.nth !operands 0) in
      operands := pop_front !operands;
      while List.length !operands > 0 do
        let ex = List.nth !operands 0 in
        operands := pop_front !operands;
        result_expr := Binary (!result_expr, op, ex)
      done;
      !result_expr
  | Minus -> deep_optimize_binary_expr ex1 Plus (optimize_unary_expr Minus ex2)
  | _ -> Binary (ex1, op, ex2)

and optimize_left_expr (const : int) (op : oper) (subex2 : expr) =
  match subex2 with
  | Number n2 -> Number (eval_binary_operation const op n2)
  | _ -> deep_optimize_binary_expr (Number const) op subex2

and optimize_unary_expr (op : oper) (subex : expr) =
  let optisubex = optimize_expr subex in
  match op with
  | Plus -> optisubex
  | Minus -> (
      match optisubex with
      | Number n -> Number (-n)
      | _ -> Unary (Minus, optisubex))
  | _ ->
      throw_except(ASTError("unexpected unary operator: " ^ string_of_binary_operator op))

and optimize_binary_expr (subex1 : expr) (op : oper) (subex2 : expr) =
  let optisubex1 = optimize_expr subex1 in
  let optisubex2 = optimize_expr subex2 in
  match optisubex1 with
  | Number n1 -> optimize_left_expr n1 op optisubex2
  | _ -> deep_optimize_binary_expr optisubex1 op optisubex2

and optimize_expr (ex : expr) =
  match ex with
  | Variable _ -> ex
  | Number _ -> ex
  | Unary (op, subex) -> optimize_unary_expr op subex
  | AssignExpression (v, op, subex) ->
      AssignExpression (v, op, optimize_expr subex)
  | Binary (subex1, op, subex2) -> optimize_binary_expr subex1 op subex2
  | FuncCall (_, _) -> ex
  | EmptyExpression -> ex

let rec optimize_stmt (stmt : statement) =
  match stmt with
  | Expression ex -> Expression (optimize_expr ex)
  | AssignStatement (v, ex) -> AssignStatement (v, optimize_expr ex)
  | While (ex, stmts) -> While (optimize_expr ex, optimize_stmts stmts)
  | If (ex, then_stmts, else_stmts) ->
      If (optimize_expr ex, optimize_stmts then_stmts, optimize_stmts else_stmts)
  | ReturnStatement _ -> stmt
  | EmptyStatement -> stmt

and optimize_stmts (stmts : statement list) : statement list =
  let new_stmts = ref [] in
  List.iter (fun stmt -> new_stmts := !new_stmts @ [ optimize_stmt stmt ]) stmts;
  !new_stmts

let optimize_ast (ast : statement list) : statement list =
  print_endline (string_of_statements ast);
  let new_ast = optimize_stmts ast in
  print_endline (string_of_statements new_ast);
  new_ast
