open Compiler.Parser

module Main = struct
  let text = "var a:= 10; var b:= 20;"
  let pos = ref 0
  let res = parse_program text pos

  (* let el = if List.length res > 0 then Some (List.nth res 0) else None

     let rec expr_and_statements_from_statement = function
       | While (e, sl) -> (Some e, Some sl, None)
       | If (e, sl1, sl2) -> (Some e, Some sl1, Some sl2)
       | Expression e -> (Some e, None, None)
       | AssignStatement (_, e) -> (Some e, None, None)
       | _ -> failwith "Error"

     and get_expr_and_statements el =
       match el with
       | Some el -> expr_and_statements_from_statement el
       | None -> failwith "No statement to process"

     let result = get_expr_and_statements el

     let split_for_assembler = function
       | _, Some stmts1, None ->
           List.iter
             (fun stmt -> string_of_statement text pos stmt |> print_endline)
             stmts1
       | Some e1, _, _ -> string_of_expression text pos e1 |> print_endline
       | _, Some stmts1, Some stmts2 ->
           List.iter
             (fun stmt -> string_of_statement text pos stmt |> print_endline)
             stmts1;
           List.iter
             (fun stmt -> string_of_statement text pos stmt |> print_endline)
             stmts2
       | None, _, _ -> failwith "Error: first element is None"

     let v = split_for_assembler result
  *)

  let rec stmt_to_asm s =
    let st_stack_pointer = ref 16 in
    let cur_stack_pointer = ref 16 in
    match s with
    | Expression e1 -> expr_to_asm e1 cur_stack_pointer st_stack_pointer
    | AssignStatement (_, e1) ->
        expr_to_asm e1 cur_stack_pointer st_stack_pointer
    | While (e1, _) -> expr_to_asm e1 cur_stack_pointer st_stack_pointer
    | If (e1, _, _) -> expr_to_asm e1 cur_stack_pointer st_stack_pointer
    | _ -> failwith "TO DO"

  and expr_to_asm e cur_stack_pointer st_stack_pointer =
    match e with
    | Number n ->
        cur_stack_pointer := !cur_stack_pointer + 4;
        "li a5, " ^ string_of_int n ^ "\n" ^ "sw a5, -"
        ^ string_of_int !cur_stack_pointer
        ^ "(s0)"
    | Unary (o, ex) -> (
        match o with
        | Minus ->
            expr_to_asm ex cur_stack_pointer st_stack_pointer ^ "\nneg a5, a5"
        | Plus -> expr_to_asm ex cur_stack_pointer st_stack_pointer
        | _ -> "")
    | Binary (ex1, o, ex2) -> (
        let asm1 = expr_to_asm ex1 cur_stack_pointer st_stack_pointer in
        let asm2 = expr_to_asm ex2 cur_stack_pointer st_stack_pointer in
        match o with
        | Plus ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            st_stack_pointer := !st_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!st_stack_pointer + 4)
            ^ "\naddw a5, a4, a5" ^ "\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
        | More ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            st_stack_pointer := !st_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!st_stack_pointer + 4)
            ^ "\nsgt a5, a4, a5" ^ "\n" ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
        | Low ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            st_stack_pointer := !st_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!st_stack_pointer + 4)
            ^ "\nslt a5, a4, a5" ^ "\n" ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
        | Equal ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            st_stack_pointer := !st_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!st_stack_pointer + 4)
            ^ "\nsub a5, a4, a5" ^ "\nseqz a5, a4, a5" ^ "\n"
            ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
        | Unequal ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            st_stack_pointer := !st_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!st_stack_pointer + 4)
            ^ "\nsub a5, a4, a5" ^ "\nsnez a5, a4, a5" ^ "\n"
            ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
        | _ -> "<no info>")
    | AssignExpression (_, DefaultAssign, ex2) ->
        expr_to_asm ex2 cur_stack_pointer st_stack_pointer
    (* | AssignExpression (_, o, ex2) -> (
        let asm2 = expr_to_asm ex2 cur_stack_pointer st_stack_pointer in
        match o with
        | PlusAssign ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            st_stack_pointer := !st_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!st_stack_pointer + 4)
            ^ "\naddw a5, a4, a5" ^ "\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
        | _ -> "<no info>") *)
    | _ -> "<no info>"

  let text = "while 10 == 15 do a += 15; done"
  let pos = ref 0
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

  let () =
    let e = parse_statements text pos check_program_end in
    List.nth e 0 |> stmt_to_asm |> print_endline
end
