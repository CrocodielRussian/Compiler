open Compiler.Parser

module Main = struct
  (* let text = "while n > 10 do a+=1; done"
     let pos = ref 0
     let res = parse_program text pos *)

  let rec expr_to_asm e cur_stack_pointer shift costl =
    match e with
    | Number n ->
        cur_stack_pointer := !cur_stack_pointer + 4;
        "li a5, " ^ string_of_int n ^ "\n" ^ "sw a5, -"
        ^ string_of_int !cur_stack_pointer
        ^ "(s0)"
    | Unary (o, ex) -> (
        match o with
        | Minus -> expr_to_asm ex cur_stack_pointer shift costl ^ "\nneg a5, a5"
        | Plus -> expr_to_asm ex cur_stack_pointer shift costl
        | _ -> "")
    | Binary (ex1, o, ex2) -> (
        let asm1 = expr_to_asm ex1 cur_stack_pointer shift costl in
        let asm2 = expr_to_asm ex2 cur_stack_pointer shift costl in
        match o with
        | Plus ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            shift := !shift + 4 + (4 * !costl);
            costl := 1;
            (* Это костыль, за который мне стыдно:()*)
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int (!cur_stack_pointer - (4 + !shift))
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!cur_stack_pointer - !shift)
            ^ "(s0)" ^ "\naddw a5, a4, a5" ^ "\n" ^ "sw a5, -"
            ^ string_of_int (!cur_stack_pointer - (4 + !shift))
            ^ "(s0)"
        | More ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
            ^ string_of_int (!cur_stack_pointer + 4)
            ^ "\nsgt a5, a4, a5" ^ "\n" ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
            ^ string_of_int !cur_stack_pointer
            ^ "(s0)"
            (*| Low ->
                   cur_stack_pointer := !cur_stack_pointer + 4;
                   asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
                   ^ string_of_int !st_stack_pointer
                   ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
                   ^ string_of_int (!st_stack_pointer + 4)
                   ^ "\nslt a5, a4, a5" ^ "\n" ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
                   ^ string_of_int !cur_stack_pointer
                   ^ "(s0)"
               | Equal ->
                   cur_stack_pointer := !cur_stack_pointer + 4;
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
                   asm1 ^ "\n" ^ asm2 ^ "\nlw a5, -"
                   ^ string_of_int !st_stack_pointer
                   ^ "(s0)\n" ^ "mv a4, a5\n" ^ "lw a5, -"
                   ^ string_of_int (!st_stack_pointer - 4)
                   ^ "\nsub a5, a4, a5" ^ "\nsnez a5, a4, a5" ^ "\n"
                   ^ "andi a5, a5, 0xff\n" ^ "sw a5, -"
                   ^ string_of_int !cur_stack_pointer
                   ^ "(s0)" *)
        | _ -> "<no-info>")
    | AssignExpression (_, DefaultAssign, ex2) ->
        expr_to_asm ex2 cur_stack_pointer shift costl
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

  let rec stmt_to_asm s cur_stack_pointer shift costl =
    match s with
    | Expression e1 -> expr_to_asm e1 cur_stack_pointer shift costl
    | AssignStatement (_, e1) -> expr_to_asm e1 cur_stack_pointer shift costl
    | While (e1, stmt1) -> stmts_to_asm e1 stmt1 cur_stack_pointer shift costl
    | If (e1, _, _) -> expr_to_asm e1 cur_stack_pointer shift costl
    | _ -> failwith "TO DO"

  and stmts_to_asm expression statements cur_stack_pointer shift costl =
    let e1 = expr_to_asm expression cur_stack_pointer shift costl in
    List.fold_left
      (fun acc stmt -> acc ^ stmt_to_asm stmt cur_stack_pointer shift costl)
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
  let text = "while 10 > 20 do 10 + 12; done"
  let pos = ref 0
  let shift = ref 0
  let costl = ref 0
  let cur_stack_pointer = ref 16

  let () =
    let e = parse_program text pos in
    List.iter
      (fun stmt ->
        stmt_to_asm stmt cur_stack_pointer shift costl |> print_endline)
      e
end
