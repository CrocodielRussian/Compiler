open Compiler.Parser

module Main = struct
  let text = "if a < 10 then a += 20; else b += 30; endif"
  let pos = ref 0
  let res = parse_program text pos
  let el = if List.length res > 0 then Some (List.nth res 0) else None

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
  let file = "bin/first.s"
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
    close_out oc
end
