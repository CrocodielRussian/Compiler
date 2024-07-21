open Compiler.Parser

module Main = struct
  module StringMap = Map.Make (String)

  let init_variables (cur_stack_pointer : int ref) (statements : statement list)
      =
    let variables_stack_position : int StringMap.t ref = ref StringMap.empty in
    List.iter
      (fun stmt ->
        match stmt with
        | AssignStatement (v, _) ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            variables_stack_position :=
              StringMap.add v !cur_stack_pointer !variables_stack_position
        | _ -> ())
      statements;
    !variables_stack_position

  let variables_shifts : int StringMap.t ref = ref StringMap.empty

  let rec binop_to_asm op =
    match op with
    | Plus -> "addw a5, a4, a5"
    | Minus -> "subw a5, a4, a5"
    | Multiply -> "mulw a5, a4, a5"
    | Divide -> "div a5, a4, a5"
    | More -> "sgt a5, a4, a5"
    | MoreOrEqual -> binop_to_asm Low ^ "\nxori a5, a5, 1"
    | Low -> "slt a5, a4, a5"
    | LowOrEqual -> binop_to_asm More ^ "\nxori a5, a5, 1"
    | Equal -> binop_to_asm Unequal ^ "\nxori a5, a5, 1"
    | Unequal -> "sne a5, a4, a5"
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
        | DefaultAssign ->
            expr_asm ^ "\nsw a5, -" ^ string_of_int var_pos ^ "(s0)"
        | _ ->
            let op_asm = binop_to_asm (map_assign_op_to_binop op) in
            expr_asm ^ "\nlw a4, -" ^ string_of_int var_pos ^ "(s0)" ^ op_asm
            ^ "\nsw a5, -" ^ string_of_int var_pos ^ "(s0)")
    | Number n -> "li a5, " ^ string_of_int n
    | Variable v ->
        let var_pos = StringMap.find v !variables_shifts in
        "lw a5, -" ^ string_of_int var_pos ^ "(s0)"
    | Unary (op, ex) -> (
        let ex_asm = expr_to_asm ex cur_stack_pointer in
        match op with
        | Plus -> ex_asm
        | Minus -> ex_asm ^ "\nneg a5, a5"
        | _ ->
            failwith
              ("ASTError: unexpected unary operator: "
              ^ string_of_binary_operator op
              ^ "."))
    | Binary (ex1, op, ex2) ->
        let asm1 = expr_to_asm ex1 cur_stack_pointer in
        cur_stack_pointer := !cur_stack_pointer + 4;
        let asm2 = expr_to_asm ex2 cur_stack_pointer in
        let full_asm =
          asm1 ^ "\nsw a5, -"
          ^ string_of_int !cur_stack_pointer
          ^ "(s0)" ^ "\n" ^ asm2 ^ "\n" ^ "lw a4, -"
          ^ string_of_int !cur_stack_pointer
          ^ "(s0)"
        in
        cur_stack_pointer := !cur_stack_pointer - 4;
        full_asm ^ "\n" ^ binop_to_asm op
    | EmptyExpression -> "# Empty expression"

  let parse_of_condition e cur_stack_pointer count_of_while =
    match e with
    | Binary (ex1, o, ex2) -> (
        let asm1 = expr_to_asm ex1 cur_stack_pointer in
        cur_stack_pointer := !cur_stack_pointer + 4;
        let asm2 = expr_to_asm ex2 cur_stack_pointer in
        let full_asm =
          asm1 ^ "\nsw a5, -"
          ^ string_of_int !cur_stack_pointer
          ^ "(s0)" ^ "\n" ^ asm2 ^ "\n" ^ "lw a4, -"
          ^ string_of_int !cur_stack_pointer
          ^ "(s0)"
        in
        match o with
        | Low ->
            full_asm ^ "\nblt a4, a5, .while_loop"
            ^ string_of_int !count_of_while
        | More ->
            full_asm ^ "\nbgt a4, a5, .while_loop"
            ^ string_of_int !count_of_while
        | LowOrEqual ->
            full_asm ^ "\ble a4, a5, .while_loop"
            ^ string_of_int !count_of_while
        | MoreOrEqual ->
            full_asm ^ "\nbge a4, a5, .while_loop"
            ^ string_of_int !count_of_while
        | Equal ->
            full_asm ^ "\beq a4, a5, .while_loop"
            ^ string_of_int !count_of_while
        | Unequal ->
            full_asm ^ "\nbne a4, a5, .while_loop"
            ^ string_of_int !count_of_while
        | _ -> failwith "TO DO")
    | _ -> failwith "TO DO"

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
        (* var <v> = <ex>*)
        let var_pos = StringMap.find v !variables_shifts in
        let asm = expr_to_asm ex cur_stack_pointer in
        asm ^ "\nsw a5, -" ^ string_of_int var_pos ^ "(s0)"
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
    "j .while_condition_"
    ^ string_of_int !count_of_while
    ^ "\n\n" ^ ".while_loop_"
    ^ string_of_int !count_of_while
    ^ ":\n"
    ^ stmts_to_asm stmts cur_stack_pointer count_of_while count_of_if
        open_label_count
    ^ "\n\n.while_condition_"
    ^ string_of_int !count_of_while
    ^ ":\n"
    ^ parse_of_condition e cur_stack_pointer count_of_while

  and stmts_to_asm stmts cur_stack_pointer count_of_while count_of_if
      open_label_count =
    let stmts_asm = ref "" in
    List.iter
      (fun stmt ->
        stmts_asm :=
          !stmts_asm
          ^ stmt_to_asm stmt cur_stack_pointer count_of_while count_of_if
              open_label_count)
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
    let else_branch_label_name =
      ".if_" ^ string_of_int current_if_index ^ "_else"
    in
    let next_open_label_name = ".L" ^ string_of_int current_open_label_index in
    ex_asm ^ "\nben a5, zero, " ^ else_branch_label_name ^ "\n" ^ then_stmts_asm
    ^ "\nj " ^ next_open_label_name ^ "\n\n" ^ else_branch_label_name ^ ":\n"
    ^ else_stmts_asm ^ "\nj " ^ next_open_label_name ^ "\n\n"
    ^ next_open_label_name ^ ":"

  (* let text = "var a := 1; var b := 2; while 10 < 20 do 10 + 12; done var c := 3;" *)

  (* let text = "var n := 1; while n < 2 do n := n + -1; done" *)

  (* let text =
     "var n := 1; if n < 2 then n := n + -1; else n := n - 1; endif if n < 2 \
      then n := n + 1; else n := n * -1; endif" *)

  let text =
    "var n := 0; if n then if n < 10 then n := n + 10; endif  else n := n * \
     -1; endif"

  let pos = ref 0
  let shift = ref 0
  let costl = ref 0
  let cur_stack_pointer = ref 16
  let st_stack_pointer = ref 16
  let count_of_while = ref 0
  let count_of_if = ref 0
  let open_label_count = ref 0

  let () =
    let statements = parse_program text pos in
    variables_shifts := init_variables st_stack_pointer statements;

    (* StringMap.iter
         (fun key value -> print_endline (key ^ ": " ^ string_of_int value))
         !variables_shifts;

       print_endline "======="; *)
    print_endline
      ".global _start\n\
       _start:\n\
       addi sp, sp, -32\n\
       sd s0, 24(sp)\n\
       addi s0, sp, 32\n\
       # START CODE";
    cur_stack_pointer := !st_stack_pointer;
    List.iter
      (fun stmt ->
        stmt_to_asm stmt cur_stack_pointer count_of_while count_of_if
          open_label_count
        |> print_endline)
      statements

  let end_call =
    print_endline
      "# END CODE\naddi sp, sp, 32\nli a0, 0\nmv a5, a0\nli a7, 93\necall"
end

(* let file = "bin/first.s"
     let text = ".global _start\n _start:\n"
     let count_of_var = 5
     let count_of_if = 0
     let size_of_buffer = 16 + (count_of_var / 4 * 16)
     let size_of_stack text size_of_buffer =
          text ^ "addi sp, sp, -"
          ^ string_of_int size_of_buffer
          ^ "\n" ^ "sd s0, "
          ^ string_of_int (size_of_buffer - 8)
          ^ "(sp)" ^ "\n" ^ "addi s0, sp,"
          ^ string_of_int size_of_buffer
          ^ "\n"

        let text = size_of_stack text size_of_buffer
        let start = 20

        (* let add_var_in_stack =  *)

        let () =
          let oc = open_out file in
          Printf.fprintf oc "%s\n" text;
          close_out oc *)