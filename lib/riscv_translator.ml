open Parser

(* open Asm_tree *)
module StringMap = Map.Make (String)

let init_variables (cur_stack_pointer : int ref) (statements : statement list) =
  let variables_stack_position : int StringMap.t ref = ref StringMap.empty in
  List.iter
    (fun stmt ->
      match stmt with
      | AssignStatement (v, _) ->
          cur_stack_pointer := !cur_stack_pointer + 8;
          variables_stack_position :=
            StringMap.add v !cur_stack_pointer !variables_stack_position
      | _ -> ())
    statements;
  !variables_stack_position

let variables_shifts : int StringMap.t ref = ref StringMap.empty

let rec binop_to_asm op =
  match op with
  | Plus -> "add a5, a4, a5"
  | Minus -> "sub a5, a4, a5"
  | Multiply -> "mul a5, a4, a5"
  | Divide -> "div a5, a4, a5"
  | More -> "sgt a5, a4, a5"
  | MoreOrEqual -> Printf.sprintf "%s\nxori a5, a5, 1" (binop_to_asm Low)
  | Low -> "slt a5, a4, a5"
  | LowOrEqual -> Printf.sprintf "%s\nxori a5, a5, 1" (binop_to_asm More)
  | Equal -> "sub a5, a4, a5\nseqz a5, a5"
  | Unequal -> Printf.sprintf "%s\nxori a5, a5, 1" (binop_to_asm Equal)
  | Invalid ->
      failwith
        ("ASTError: unexpected binary operator: "
        ^ string_of_binary_operator op
        ^ ".")

let map_assign_op_to_binop op =
  match op with
  | DefaultAssign -> Invalid
  | PlusAssign -> Plus
  | MinusAssign -> Minus
  | MultiplyAssign -> Multiply
  | DivideAssign -> Divide
  | InvalidAssing ->
      failwith
        ("ASTError: unexpected assign operator: "
        ^ string_of_assign_operator op
        ^ ".")

let rec expr_to_asm e cur_stack_pointer =
  match e with
  | AssignExpression (v, op, ex) -> (
      let var_pos = StringMap.find v !variables_shifts in
      let expr_asm = expr_to_asm ex cur_stack_pointer in
      match op with
      | DefaultAssign -> Printf.sprintf "%s\nsd a5, -%d(s0)" expr_asm var_pos
      | _ ->
          let op_asm = binop_to_asm (map_assign_op_to_binop op) in
          Printf.sprintf "%s\nld a4, -%d(s0)\n%s\nsd a5, -%d(s0)" expr_asm
            var_pos op_asm var_pos)
  | Number n -> Printf.sprintf "li a5, %d" n
  | Variable v ->
      let var_pos = StringMap.find v !variables_shifts in
      Printf.sprintf "ld a5, -%d(s0)" var_pos
  | Unary (op, ex) -> (
      let ex_asm = expr_to_asm ex cur_stack_pointer in
      match op with
      | Plus -> ex_asm
      | Minus -> Printf.sprintf "%s\nneg a5, a5" ex_asm
      | _ ->
          failwith
            ("ASTError: unexpected unary operator: "
            ^ string_of_binary_operator op
            ^ "."))
  | Binary (ex1, op, ex2) ->
      let asm1 = expr_to_asm ex1 cur_stack_pointer in
      cur_stack_pointer := !cur_stack_pointer + 8;
      let asm2 = expr_to_asm ex2 cur_stack_pointer in
      let full_asm =
        Printf.sprintf "%s\nsd a5, -%d(s0)\n%s\nld a4, -%d(s0)" asm1
          !cur_stack_pointer asm2 !cur_stack_pointer
      in
      cur_stack_pointer := !cur_stack_pointer - 4;
      Printf.sprintf "%s\n%s" full_asm (binop_to_asm op)
  | EmptyExpression -> "# Empty expression"

let rec stmt_to_asm s cur_stack_pointer count_of_while count_of_if
    open_label_count =
  match s with
  | Expression ex -> (
      match ex with
      | AssignExpression (_, _, _) -> expr_to_asm ex cur_stack_pointer
      | _ ->
          failwith
            ("ASTError: unsupported  expression statement: "
            ^ string_of_expression ex ex ex
            ^ ";."))
  | AssignStatement (v, ex) ->
      let var_pos = StringMap.find v !variables_shifts in
      let asm = expr_to_asm ex cur_stack_pointer in
      Printf.sprintf "%s\nsd a5, -%d(s0)" asm var_pos
  | While (ex, stmts) ->
      while_loop_to_asm ex stmts cur_stack_pointer count_of_while count_of_if
        open_label_count
  | If (ex, then_stmts, else_stmts) ->
      if_stmt_to_asm ex then_stmts else_stmts cur_stack_pointer count_of_while
        count_of_if open_label_count
  | EmptyStatement -> "# Empty Statement"

and while_loop_to_asm e stmts cur_stack_pointer count_of_while count_of_if
    open_label_count =
  count_of_while := !count_of_while + 1;
  let cur_while_index = !count_of_while in
  let while_condition_label =
    Printf.sprintf ".while_%d_condition" cur_while_index
  in
  let while_loop_label = Printf.sprintf ".while_%d_loop" cur_while_index in
  let exp_while = expr_to_asm e cur_stack_pointer in
  let stmts_asm =
    stmts_to_asm stmts cur_stack_pointer count_of_while count_of_if
      open_label_count
  in
  Printf.sprintf "j %s\n\n%s:\n%s\nj %s\n\n%s:\n%s\nbne a5, zero, %s"
    while_condition_label while_loop_label stmts_asm while_condition_label
    while_condition_label exp_while while_loop_label

and stmts_to_asm stmts cur_stack_pointer count_of_while count_of_if
    open_label_count =
  let stmts_asm = ref "" in
  List.iter
    (fun stmt ->
      let stmt_asm =
        stmt_to_asm stmt cur_stack_pointer count_of_while count_of_if
          open_label_count
      in
      if String.length stmt_asm > 0 then
        stmts_asm := Printf.sprintf "%s\n%s" !stmts_asm stmt_asm
      else ())
    stmts;
  !stmts_asm

and if_stmt_to_asm ex then_stmts else_stmts cur_stack_pointer count_of_while
    count_of_if open_label_count =
  incr count_of_if;
  incr open_label_count;
  let current_open_label_index = !count_of_if in
  let current_if_index = !count_of_if in
  let ex_asm = expr_to_asm ex cur_stack_pointer in
  let then_stmts_asm =
    stmts_to_asm then_stmts cur_stack_pointer count_of_while count_of_if
      open_label_count
  in
  let else_stmts_asm =
    stmts_to_asm else_stmts cur_stack_pointer count_of_while count_of_if
      open_label_count
  in
  let else_branch_label_name = Printf.sprintf ".if_%d_else" current_if_index in
  let next_open_label_name = ".L" ^ string_of_int current_open_label_index in
  Printf.sprintf "%s\nbeq a5, zero, %s\n%s\nj %s\n\n%s:\n%s\nj %s\n\n%s:" ex_asm
    else_branch_label_name then_stmts_asm next_open_label_name
    else_branch_label_name else_stmts_asm next_open_label_name
    next_open_label_name

let cur_stack_pointer = ref 16
let st_stack_pointer = ref 16
let count_of_while = ref 0
let count_of_if = ref 0
let open_label_count = ref 0

let asm_translator text =
  let statements = parse_program text in
  variables_shifts := init_variables st_stack_pointer statements;
  let space_stack = 16 + !st_stack_pointer in
  let start_code =
    Printf.sprintf
      ".global _start\n\
       _start:\n\
       addi sp, sp, -%d\n\
       sd s0, %d(sp)\n\
       addi s0, sp, %d\n\
       # START CODE" space_stack (space_stack - 8) space_stack
  in
  print_endline start_code;
  cur_stack_pointer := !st_stack_pointer;
  List.iter
    (fun stmt ->
      let stmt_asm =
        stmt_to_asm stmt cur_stack_pointer count_of_while count_of_if
          open_label_count
      in
      print_endline stmt_asm)
    statements;
  let end_code =
    Printf.sprintf
      "# END CODE\naddi sp, sp, %d\nli a0, 0\nmv a5, a0\nli a7, 93\necall"
      space_stack
  in
  print_endline end_code
