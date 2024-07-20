open Compiler.Parser

module Main = struct
  module StringMap = Map.Make (String)

  let variables_shifts : int StringMap.t ref = ref StringMap.empty

  let rec expr_to_asm e cur_stack_pointer st_stack_pointer =
    match e with
    (* | AssignExpression (v, op, ex) -> (
        let var_shift = StringMap.find v !variables_shifts in
        let expr_asm = expr_to_asm ex cur_stack_pointer shift costl in
        match op with
        | DefaultAssign ->
            expr_asm ^ "\n" ^ "sw a5, -" ^ string_of_int var_shift ^ "(s0)"
        | PlusAssign ->
            expr_asm ^ "\n" ^ "lw a4, -" ^ string_of_int var_shift ^ "(s0)\n"
            ^ "addw a5, a4, a5\n" ^ "sw a5, -" ^ string_of_int var_shift
            ^ "(s0)"
        | MinusAssign ->
            expr_asm ^ "\n" ^ "lw a4, -" ^ string_of_int var_shift ^ "(s0)\n"
            ^ "subw a5, a4, a5\n" ^ "sw a5, -" ^ string_of_int var_shift
            ^ "(s0)"
        | MultiplyAssign ->
            expr_asm ^ "\n" ^ "lw a4, -" ^ string_of_int var_shift ^ "(s0)\n"
            ^ "mulw a5, a4, a5\n" ^ "sw a5, -" ^ string_of_int var_shift
            ^ "(s0)"
        | DivideAssign ->
            expr_asm ^ "\n" ^ "lw a4, -" ^ string_of_int var_shift ^ "(s0)\n"
            ^ "divw a5, a4, a5\n" ^ "sw a5, -" ^ string_of_int var_shift
            ^ "(s0)"
        | InvalidAssing ->
            failwith "AST Error: unexpected assign expression type") *)
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
            let text = asm1 ^ "\n" ^ asm2 ^ "\n" ^ "li a5, zero" ^ "\n" in
            let buf = Buffer.create 1024 in
            while !st_stack_pointer < !cur_stack_pointer do
              st_stack_pointer := !st_stack_pointer + 4;
              Buffer.add_string buf
                (Printf.sprintf "ld a4, -%d(s0)\naddi a5, a5, a4\n"
                   !st_stack_pointer)
            done;
            st_stack_pointer := !st_stack_pointer + 4;
            cur_stack_pointer := !st_stack_pointer;
            text ^ Buffer.contents buf ^ "sw a5, -"
            ^ string_of_int !st_stack_pointer
            ^ "(s0)\n"
        (* ld a4, -20(s0)
           addi a5, a5, a4
           ld a4, -24(s0)
           addi a5, a5, a4
           ld a4, -28(s0)
           addi a5, a5, a4
           sw a5, -32(s0) *)
        | _ -> "<no-info>")
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

  let rec stmt_to_asm s cur_stack_pointer st_stack_pointer =
    match s with
    | Expression e1 -> expr_to_asm e1 cur_stack_pointer st_stack_pointer
    | AssignStatement (_, e1) ->
        expr_to_asm e1 cur_stack_pointer st_stack_pointer
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
  let text = "var acc:= 7; var n:=1 while do n := n - 1; "
  let pos = ref 0
  let shift = ref 0
  let costl = ref 0
  let cur_stack_pointer = ref 16
  let st_stack_pointer = ref 16

  let () =
    let e = parse_program text pos in
    List.iter
      (fun stmt ->
        stmt_to_asm stmt cur_stack_pointer st_stack_pointer |> print_endline)
      e
end
