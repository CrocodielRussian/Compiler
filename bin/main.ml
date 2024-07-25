open Compiler.Riscv_translator
open Compiler.Asm_tree
open Compiler.Parser

module Main = struct
  let text =
    "var a := 10; var b := 20; var c := 20; while a > 10 do a += 2; done"

  let cur_stack_pointer = ref 16
  let fin = asm_tree (parse_program text) cur_stack_pointer
  let () = asm_translator text
end
