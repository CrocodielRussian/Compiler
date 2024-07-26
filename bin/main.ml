open Compiler.Asm_tree
open Compiler.Parser

module Main = struct
  let text =
    "var a := 10; var b := 20; while b - a do b*=a; if (b / a) then a += 1; \
     else b -= 1; endif done a := b /(2 + a);"

  let () =
    let instructions = program_to_asm_tree (parse_program text) in
    List.iter
      (fun instruction -> print_endline (show_instr instruction))
      instructions
end
