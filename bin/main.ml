(* open Compiler.Asm_tree *)
open Compiler.Parser
(* open Compiler.Riscv_translator *)

module Main = struct
  let pos = ref 0
  let text =
    "def hello(a){a += 10; return a;} def main(){var b := 0; hello(10); return 0;}  "

  let () =
    List.iter(fun st -> show_structure st |> print_endline ) (parse_structures text pos check_func_stmts_end)
    (* let instructions = program_to_asm_tree (parse_program text) in *)
    (* print_endline (string_of_statements (parse_program text)) *)
  (* List.iter
     (fun instruction -> print_endline (show_instr instruction))
     instructions; *)
  (* print_endline (string_of_instr_list instructions) *)
end
