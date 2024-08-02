(* This file included of optimize AST Oberon-like: const_opmize and dead_code_optimize*)
open Parser
open Exceptions

let initialised_variables = ref StringMap.(empty)
let changeable_variables= ref StringSet.(empty)
let bool_to_int = function true -> 1 | false -> 0
let bool_of_int num = num != 0

(* 
   [n1] - left number of binary
   [n2] - right number of binary
   [op] - operation wiht that numbers

   Return: const optimizer number
*)

let eval_binary_operation (n1 : int) (op : oper) (n2 : int) =
  match op with
  | Plus -> n1 + n2
  | Minus -> n1 - n2
  | Multiply -> n1 * n2
  | Divide -> (
      match n2 with
      | 0 -> throw_except (LogicErrorMath "divide by zero")
      | _ -> n1 / n2)
  | Mod -> (
      match n2 with
      | 0 -> throw_except (LogicErrorMath "divide by zero")
      | _ -> n1 mod n2)
  | Low -> bool_to_int (n1 < n2)
  | LowOrEqual -> bool_to_int (n1 <= n2)
  | More -> bool_to_int (n1 > n2)
  | MoreOrEqual -> bool_to_int (n1 >= n2)
  | Equal -> bool_to_int (n1 == n2)
  | Unequal -> bool_to_int (n1 != n2)
  | AndOper -> bool_to_int (bool_of_int n1 || bool_of_int n2)
  | OrOper -> bool_to_int (bool_of_int n1 || bool_of_int n2)
  | _ -> throw_except (ASTError "unexpected operator")

let valid_op_const (op : oper) =
  match op with Plus -> 0 | Multiply -> 1 | _ -> 0

(* 
   [op] - left number of binary
   [expressions] - 

   Return: const optimizer number
*)

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

let rec deep_const_optimize_binary_expr (ex1 : expr) (op : oper) (ex2 : expr) :
    expr =
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
  | Minus ->
      deep_const_optimize_binary_expr ex1 Plus
        (const_optimize_unary_expr Minus ex2)
  | _ -> Binary (ex1, op, ex2)

and const_optimize_left_expr (const : int) (op : oper) (subex2 : expr) =
  match subex2 with
  | Number n2 -> Number (eval_binary_operation const op n2)
  | _ -> deep_const_optimize_binary_expr (Number const) op subex2

and const_optimize_unary_expr (op : oper) (subex : expr) =
  let optisubex = const_optimize_expr subex in
  match op with
  | Plus -> optisubex
  | Minus -> (
      match optisubex with
      | Number n -> Number (-n)
      | _ -> Unary (Minus, optisubex))
  | _ ->
      throw_except
        (ASTError ("unexpected unary operator: " ^ string_of_binary_operator op))

and const_optimize_binary_expr (subex1 : expr) (op : oper) (subex2 : expr) =
  let optisubex1 = const_optimize_expr subex1 in
  let optisubex2 = const_optimize_expr subex2 in
  match optisubex1 with
  | Number n1 -> const_optimize_left_expr n1 op optisubex2
  | _ -> deep_const_optimize_binary_expr optisubex1 op optisubex2

and const_optimize_expr (ex : expr) =
  match ex with
  | Variable _ -> ex
  | Number _ -> ex
  | Unary (op, subex) -> const_optimize_unary_expr op subex
  | AssignExpression (v, op, subex) ->
      AssignExpression (v, op, const_optimize_expr subex)
  | Binary (subex1, op, subex2) -> const_optimize_binary_expr subex1 op subex2
  | FuncCall (name, expressions) ->
      let optiexpression = ref [] in
      List.iter
        (fun expression ->
          optiexpression := !optiexpression @ [ const_optimize_expr expression ])
        expressions;
      FuncCall (name, !optiexpression)
  | EmptyExpression -> ex

let rec const_optimize_stmt (stmt : statement) =
  match stmt with
  | Expression ex -> Expression (const_optimize_expr ex)
  | AssignStatement (v, ex) -> AssignStatement (v, const_optimize_expr ex)
  | While (ex, stmts) ->
      While (const_optimize_expr ex, const_optimize_stmts stmts)
  | If (ex, then_stmts, else_stmts) ->
      If
        ( const_optimize_expr ex,
          const_optimize_stmts then_stmts,
          const_optimize_stmts else_stmts )
  | ReturnStatement expr -> ReturnStatement (const_optimize_expr expr)
  | BreakStatement -> stmt
  | EmptyStatement -> stmt

and const_optimize_stmts (stmts : statement list) : statement list =
  let new_stmts = ref [] in
  List.iter
    (fun stmt -> new_stmts := !new_stmts @ [ const_optimize_stmt stmt ])
    stmts;
  !new_stmts

and check_changer_op op  =   
  match op with
  | DefaultAssign -> true
  | PlusAssign -> true
  | MultiplyAssign -> true
  | DivideAssign -> true
  | MinusAssign -> true
  | InvalidAssing -> false

and unused_stmt_optimize_assign_statement var ex = 
  match ex with
  | Number n -> initialised_variables := StringMap.add var n !initialised_variables
  | _ -> ()
  
and check_binary_operation (n1 : int) (op : oper) (n2 : int) =
  match op with
  | Low -> (n1 < n2)
  | LowOrEqual -> (n1 <= n2)
  | More -> (n1 > n2)
  | MoreOrEqual -> (n1 >= n2)
  | Equal -> (n1 == n2)
  | Unequal -> (n1 != n2)
  | _ -> throw_except (ASTError "unexpected operator")

and unused_stmt_optimize_binary_expression ex1 op ex2 = 
  match ex1, ex2 with
  | Variable v, Number n2 -> 
  (
    if StringMap.mem v !initialised_variables then
    (
      let n1 = StringMap.find v !initialised_variables in check_binary_operation n1 op n2
    )
    else 
    (
      false
    )
  )                 
  | Number n1, Number n2 -> check_binary_operation n1 op n2
  | _ -> false
and unused_stmt_optimize_expression (ex : expr) : bool =
  match ex with
  | Number n -> bool_of_int n
  | Binary (ex1, op, ex2) -> unused_stmt_optimize_binary_expression ex1 op ex2
  | _ -> failwith "unexpected expression"

and check_of_changeable_variables_in_expression ex = 
  match ex with
  | AssignExpression(v,_,_) -> changeable_variables := StringSet.add v !changeable_variables
  | _ -> ()
and check_of_changeable_variables_in_assign_statement v ex = 
  match ex with
  | AssignExpression(_,_,_) -> changeable_variables := StringSet.add v !changeable_variables
  | _ -> ()
and check_of_changeable_variables_in_stmt stmt = 
  match stmt with
  | AssignStatement(v, ex) -> check_of_changeable_variables_in_assign_statement v ex
  | If(_, if_stmts, else_stmts) ->
  (
    check_of_changeable_variables_in_stmts if_stmts;
    check_of_changeable_variables_in_stmts else_stmts
  )
  | While(_, while_stmts) ->
    (
      check_of_changeable_variables_in_stmts while_stmts;
    )
  | Expression ex -> check_of_changeable_variables_in_expression ex
  | _ -> ()
and check_of_changeable_variables_in_stmts stmts = 
  List.iter(fun stmt -> check_of_changeable_variables_in_stmt stmt) stmts

and check_of_changeable_variables stmt = 
  match stmt with
  | AssignStatement (v, ex) -> 
  (
    check_of_changeable_variables_in_assign_statement v ex;
  )
  | If (_, if_stmts, else_stmts) -> 
  (
    check_of_changeable_variables_in_stmts if_stmts; 
    check_of_changeable_variables_in_stmts else_stmts
  )
  | While (_, while_stmts) -> check_of_changeable_variables_in_stmts while_stmts; 
  | _ -> ()
    
and unused_stmt_optimize_stmts (stmts : statement list) : statement list = 
  let new_stmts = ref [] in
  
  List.iter (fun stmt -> check_of_changeable_variables stmt) stmts;
  List.iter (fun stmt -> new_stmts := !new_stmts @ [unused_stmt_return stmt]) stmts;
  !new_stmts
and reverse_binary_operation op = 
  match op with
  | Low -> MoreOrEqual
  | LowOrEqual -> More
  | More -> LowOrEqual
  | MoreOrEqual -> Low
  | Equal -> Unequal
  | Unequal -> Equal
  | _ -> throw_except (ASTError "unexpected operator")

and unused_stmt_reverse_expression (ex : expr) : expr =
  match ex with
  | Number n -> if n == 0 then (Number 1) else (Number 0)
  | Binary (ex1, op, ex2) -> Binary(ex1, reverse_binary_operation op, ex2)
  | _ -> failwith "unexpected expression"

and check_changeable_variable_in_binary_operation ex1 = 
  match ex1 with
  | Variable v -> StringSet.mem v !changeable_variables
  | _ -> false

and check_changeable_variable_in_ex ex = 
  match ex with
  | Number _ -> false;
  | Binary (ex1, _, _) -> check_changeable_variable_in_binary_operation ex1
  | _ -> failwith "unexpected expression"
and unused_stmt_return stmt = 
  match stmt with
  | AssignStatement (v, ex) -> 
  (
    unused_stmt_optimize_assign_statement v ex;
    stmt 
  )
  | If (ex, if_stmts, else_stmts) -> 
  (
  if unused_stmt_optimize_expression ex then
    If(ex, unused_stmt_optimize_stmts if_stmts, unused_stmt_optimize_stmts else_stmts)
  else (
      if check_changeable_variable_in_ex ex then 
      (
        If(ex, unused_stmt_optimize_stmts if_stmts, unused_stmt_optimize_stmts else_stmts)
      )
      else 
      (
      (* We must correct it*)
        if (List.nth else_stmts 0 != EmptyStatement) then
        (
          If(unused_stmt_reverse_expression ex, unused_stmt_optimize_stmts else_stmts, [EmptyStatement])
        )
        else
          EmptyStatement
      )
    )
  )
  | While (ex, while_stmts) ->
  (
  if unused_stmt_optimize_expression ex then
    While(ex, unused_stmt_optimize_stmts while_stmts)
  else 
    if check_changeable_variable_in_ex ex then
      While(ex, unused_stmt_optimize_stmts while_stmts)
    else 
      EmptyStatement
  )
  | _ -> stmt

(* let optimize_empty_if (st: structure) : structure =  *)
let const_optimize_structure (st : structure) : structure =
  match st with
  | FuncStruct (name, var_args, stmts) ->
      FuncStruct (name, var_args, const_optimize_stmts stmts)
let unused_stmt_optimize_structure (st : structure) : structure =
  match st with
  | FuncStruct (name, var_args, stmts) ->
      List.iter(fun arg -> changeable_variables := StringSet.add arg !changeable_variables) var_args;
      FuncStruct (name, var_args, unused_stmt_optimize_stmts stmts)

let optimize_structures (structures : structure list) : structure list =
  let const_optimize_structures = ref [] in
  List.iter
    (fun st -> const_optimize_structures := !const_optimize_structures @ [ const_optimize_structure st ])
    structures;
    (* List.iter(fun st -> show_structure st |> print_endline) !const_optimize_structures; *)
  let optimize_unused_stmt = ref [] in
  List.iter
  (fun st -> optimize_unused_stmt := !optimize_unused_stmt @ [ unused_stmt_optimize_structure st ])
  !const_optimize_structures;
  (* List.iter(fun st -> show_structure st |> print_endline) !optimize_unused_stmt; *)
  !optimize_unused_stmt

let optimize_ast (ast : structure list) : structure list =
  let new_ast = optimize_structures ast in
  new_ast
