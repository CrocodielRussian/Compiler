open Parser

type reg = ZERO | SP | RA | V | GP | TP | T of int | A of int | S of int

type instr =
  | Add of reg * reg * reg
  | Mul of reg * reg * reg
  | Sub of reg * reg * reg
  | Div of reg * reg * reg
  | Sgt of reg * reg * reg
  | Slt of reg * reg * reg
  | InvalidInstruction

type structure = Label of string * instr list | Variable_asm of string * instr list | 

(* List.iter
    (fun stmt ->
      let asm_tree =
        stmt_to_asm stmt cur_stack_pointer count_of_while count_of_if
          open_label_count
      in
      print_endline stmt_asm) *)

let while_loop_to_asm e stmts= 

let asm_tree stmt =
  match stmt with
  | Expression ex -> (
      match ex with
      | AssignExpression (_, _, _) -> "AssignExpr"
      | _ ->
          failwith
            ("ASTError: unsupported  expression statement: "
            ^ string_of_expression ex ex ex
            ^ ";."))
  | AssignStatement (_, _) -> "assignstatement"
  | While (e, stmts) -> while_loop_to_asm e stmts
  | If (_, _, _) -> "if"
  | EmptyStatement -> "# Empty Statement"

(* and parse_statements text pos check =
   let all = ref [] in
   while check text pos do
     skip_whitespaces text pos;
     let ident = identifier text pos in
     match ident with
     | While(_, _) ->
         let result = parse_while_loop_statement text pos in
         all := !all @ [ result ]
     | If(_, _, _) ->
         let result = parse_if_statement text pos in
         all := !all @ [ result ]
     | Variable(_, _) ->
         let result = parser_assign_statement text pos in
         all := !all @ [ result ]
     | _ ->
         pos := !pos - String.length ident;
         let result = parse_expr_statement text pos in
         all := !all @ [ result ]
   done;
   !all *)

(* let cur_stack_pointer = ref 16 in
   let st_stack_pointer = ref 16 in
   let count_of_while = ref 0 in
   let count_of_if = ref 0 in
   let open_label_count = ref 0 in

   let text =
       "var a := 10; var b := 20; var c := 20; var d := 20; var e := 20; if a > \
        20 then a += 1; else a += 10; endif" in
   let asm_translator =
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
     print_endline end_code *)
