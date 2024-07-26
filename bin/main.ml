open Compiler.Asm_tree
open Compiler.Parser

module Main = struct
  let text =
    "var b := 12 + 34; var c :=b*b/3*4; var a:= -12 * +10 - 10 / 10 + 10 + b - \
     90; while a > 10 /  (1 + 0) do a /= (a + -2); done"

  let () =
    let instructions = program_to_asm_tree (parse_program text) in
    List.iter
      (fun instruction -> print_endline (show_instr instruction))
      instructions
end
