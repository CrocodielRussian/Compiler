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
      match n2 with
      | 0 -> throw_except (LogicErrorMath "divide by zero")
      | _ -> n1 / n2)
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
  | EmptyStatement -> stmt

and const_optimize_stmts (stmts : statement list) : statement list =
  let new_stmts = ref [] in
  List.iter
    (fun stmt -> new_stmts := !new_stmts @ [ const_optimize_stmt stmt ])
    stmts;
  !new_stmts

let rec func_call_deep_optimize_expr (ex : expr)
    (result_statements : statement list ref)
    (func_call_var_indexes : int StringMap.t ref) : expr =
  match ex with
  | Unary (op, subex) ->
      let optisubex =
        func_call_deep_optimize_expr subex result_statements
          func_call_var_indexes
      in
      Unary (op, optisubex)
  | Binary (subex1, op, subex2) ->
      let optisubex1 =
        func_call_deep_optimize_expr subex1 result_statements
          func_call_var_indexes
      in
      let optisubex2 =
        func_call_deep_optimize_expr subex2 result_statements
          func_call_var_indexes
      in
      Binary (optisubex1, op, optisubex2)
  | FuncCall (name, expressions) ->
      let new_expressions = ref [] in
      List.iter
        (fun expression ->
          new_expressions :=
            !new_expressions
            @ [
                func_call_deep_optimize_expr expression result_statements
                  func_call_var_indexes;
              ])
        expressions;
      let func_call_index = ref 0 in
      if StringMap.mem name !func_call_var_indexes then
        func_call_index := StringMap.find name !func_call_var_indexes + 1;
      let result_var_name = Printf.sprintf "call_%s_%d" name !func_call_index in
      result_statements :=
        !result_statements
        @ [
            AssignStatement (result_var_name, FuncCall (name, !new_expressions));
          ];
      func_call_var_indexes :=
        StringMap.add name !func_call_index !func_call_var_indexes;
      Variable result_var_name
  | AssignExpression (v, op, subex) ->
      let optisubex =
        func_call_deep_optimize_expr subex result_statements
          func_call_var_indexes
      in
      result_statements :=
        !result_statements
        @ [ Expression (AssignExpression (v, op, optisubex)) ];
      Variable v
  | _ -> ex

let rec func_call_optimize_expr_stmt (ex : expr)
    (func_call_var_indexes : int StringMap.t ref) : statement list =
  match ex with
  | Variable _ -> []
  | Number _ -> []
  | Unary (_, subex) -> func_call_optimize_expr_stmt subex func_call_var_indexes
  | AssignExpression (v, op, subex) ->
      let result_statements = ref [] in
      let optisubex =
        func_call_deep_optimize_expr subex result_statements
          func_call_var_indexes
      in
      !result_statements @ [ Expression (AssignExpression (v, op, optisubex)) ]
  | Binary (subex1, _, subex2) ->
      func_call_optimize_expr_stmt subex1 func_call_var_indexes
      @ func_call_optimize_expr_stmt subex2 func_call_var_indexes
  | FuncCall (_, _) -> [ Expression ex ]
  | EmptyExpression -> []

let rec func_call_optimize_stmt (stmt : statement)
    (func_call_var_indexes : int StringMap.t ref) : statement list =
  match stmt with
  | Expression ex -> func_call_optimize_expr_stmt ex func_call_var_indexes
  | AssignStatement (v, subex) ->
      let result_statements = ref [] in
      let optisubex =
        func_call_deep_optimize_expr subex result_statements
          func_call_var_indexes
      in
      !result_statements @ [ AssignStatement (v, optisubex) ]
  | While (_, _) -> [ stmt ]
  | If (subex, then_branch, else_branch) ->
      let result_statements = ref [] in
      let optisubex =
        func_call_deep_optimize_expr subex result_statements
          func_call_var_indexes
      in
      !result_statements
      @ [
          If
            ( optisubex,
              func_call_optimize_stmts then_branch func_call_var_indexes,
              func_call_optimize_stmts else_branch func_call_var_indexes );
        ]
  | ReturnStatement subex ->
      let result_statements = ref [] in
      let optisubex =
        func_call_deep_optimize_expr subex result_statements
          func_call_var_indexes
      in
      !result_statements @ [ ReturnStatement optisubex ]
  | EmptyStatement -> []

and func_call_optimize_stmts (stmts : statement list)
    (func_call_var_indexes : int StringMap.t ref) : statement list =
  let new_stmts = ref [] in
  List.iter
    (fun stmt ->
      new_stmts :=
        !new_stmts @ func_call_optimize_stmt stmt func_call_var_indexes)
    stmts;
  !new_stmts

let optimize_structure (st : structure) : structure =
  match st with
  | FuncStruct (name, var_args, stmts) ->
      let func_call_var_indexes = ref StringMap.empty in
      FuncStruct
        ( name,
          var_args,
          func_call_optimize_stmts
            (const_optimize_stmts stmts)
            func_call_var_indexes )

let optimize_structures (structures : structure list) : structure list =
  let new_structures = ref [] in
  List.iter
    (fun st -> new_structures := !new_structures @ [ optimize_structure st ])
    structures;
  !new_structures

let optimize_ast (ast : structure list) : structure list =
  let new_ast = optimize_structures ast in
  List.iter (fun e -> print_endline (show_structure e)) new_ast;
  new_ast
