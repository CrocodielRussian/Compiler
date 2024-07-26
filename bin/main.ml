open Compiler.Asm_tree
open Compiler.Parser
open Compiler.Riscv_translator

module Main = struct
  let text =
    "var a := 10; print_int(a);"

  let () =
    let instructions = program_to_asm_tree (parse_program text) in
    (* print_endline (string_of_statements (parse_program text)) *)
  (* List.iter
     (fun instruction -> print_endline (show_instr instruction))
     instructions; *)
  print_endline (string_of_instr_list instructions)
end
