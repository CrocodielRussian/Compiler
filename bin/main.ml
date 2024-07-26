open Compiler.Asm_tree
open Compiler.Parser

module Main = struct
  let text =
<<<<<<< HEAD
    "var b := 12 + 34; var c :=b*b/3*4; var a:= -12 * +10 - 10 / 10 + 10 + b - \
     90; while a > 10 /  (1 + 0) do a /= (a + -2); done"
=======
    "var a := 10; var b := 20; while b - a do b*=a; if (b / a) then a += 1; \
     else b -= 1; endif done a := b /(2 + a);"
>>>>>>> 7fefa6ba5e7e400a4258b955b56689a3ac432d81

  let () =
    let instructions = program_to_asm_tree (parse_program text) in
    List.iter
      (fun instruction -> print_endline (show_instr instruction))
      instructions;
    let asm_code = asm_translator text in
    let oc = open_out file in
    Printf.fprintf oc "%s\n" asm_code;
    close_out oc
end
