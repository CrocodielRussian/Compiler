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

  let rec expr_to_asm e cur_stack_pointer st_stack_pointer =
    match e with
    | AssignExpression (v, op, ex) -> (
        let var_pos = StringMap.find v !variables_shifts in
        let expr_asm = expr_to_asm ex cur_stack_pointer st_stack_pointer in
        match op with
        | DefaultAssign ->
            expr_asm ^ "\n" ^ "sw a5, -" ^ string_of_int var_pos ^ "(s0)"
        | _ -> "")
    | Number n -> "li a5, " ^ string_of_int n
    | Variable v ->
        let var_pos = StringMap.find v !variables_shifts in
        "lw a5, -" ^ string_of_int var_pos ^ "(s0)"
    | Binary (ex1, o, ex2) -> (
        let asm1 = expr_to_asm ex1 cur_stack_pointer st_stack_pointer in
        cur_stack_pointer := !cur_stack_pointer + 4;
        let asm2 = expr_to_asm ex2 cur_stack_pointer st_stack_pointer in
        let full_asm =
          asm1 ^ "\nsw a5, -"
          ^ string_of_int !cur_stack_pointer
          ^ "(s0)" ^ "\n" ^ asm2 ^ "\n" ^ "lw a4, -"
          ^ string_of_int !cur_stack_pointer
          ^ "(s0)"
        in
        cur_stack_pointer := !cur_stack_pointer - 4;
        match o with Plus -> full_asm ^ "\naddw a5, a4, a5" | _ -> "<no-info>")
    | _ -> "<no info>"

  let rec stmt_to_asm s cur_stack_pointer st_stack_pointer =
    match s with
    | Expression e1 -> (
        match e1 with
        | AssignExpression (_, _, _) ->
            expr_to_asm e1 cur_stack_pointer st_stack_pointer
        | _ -> "")
    | AssignStatement (v, e1) ->
        (* var <v> = <e1>*)
        let var_pos = StringMap.find v !variables_shifts in
        let asm = expr_to_asm e1 cur_stack_pointer st_stack_pointer in
        asm ^ "\nsw a5, -" ^ string_of_int var_pos ^ "(s0)"
    | While (e1, stmt1) ->
        stmts_to_asm e1 stmt1 cur_stack_pointer st_stack_pointer
    | If (e1, _, _) -> expr_to_asm e1 cur_stack_pointer st_stack_pointer
    | _ -> failwith "TO DO"

  (* and while_loop_to_asm =  *)

  and stmts_to_asm expression statements cur_stack_pointer st_stack_pointer =
    let e1 = expr_to_asm expression cur_stack_pointer st_stack_pointer in
    List.fold_left
      (fun acc stmt ->
        acc ^ stmt_to_asm stmt cur_stack_pointer st_stack_pointer)
      e1 statements

  (* let file = "bin/first.s"
     let text = ".global _start\n _start:\n"
     let count_of_var = 5
     let count_of_while = 1
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

  (* "var a := 1; var b := 2; while 10 < 20 do 10 + 12; done var c := 3;" *)
  let text = "var a := 10;  var b := 20; b := 10 + 20; a := a + (20 + 30) + 40;"
  let pos = ref 0
  let shift = ref 0
  let costl = ref 0
  let cur_stack_pointer = ref 16
  let st_stack_pointer = ref 16

  let () =
    let statements = parse_program text pos in
    variables_shifts := init_variables st_stack_pointer statements;
    StringMap.iter
      (fun key value -> print_endline (key ^ ": " ^ string_of_int value))
      !variables_shifts;
    print_endline "=======";
    cur_stack_pointer := !st_stack_pointer;
    List.iter
      (fun stmt ->
        stmt_to_asm stmt cur_stack_pointer st_stack_pointer |> print_endline)
      statements;
    print_endline (string_of_int !cur_stack_pointer);
    print_endline (string_of_int !st_stack_pointer)
end
