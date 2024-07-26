(* open Compiler.Asm_tree *)
open Compiler.Parser
(* open Compiler.Riscv_translator *)

module Main = struct
  let text =
    "var b := 12 + 34; var c :=b*b/3*4- print_int    (  b / b  ); var a:= -12 \
     * +10 - 10 / 10 + 10 + b - 90; while a > 10 /  (1 + 0) do a /= (a + -2); \
     done print_int(10 20);"

  let () =
    (* let instructions = program_to_asm_tree (parse_program text) in *)
    print_endline (string_of_statements (parse_program text))
  (* List.iter
     (fun instruction -> print_endline (show_instr instruction))
     instructions; *)
  (* print_endline (string_of_instr_list instructions) *)
end
