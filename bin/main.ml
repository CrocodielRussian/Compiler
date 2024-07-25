(* open Compiler.Riscv_translator *)
open Compiler.Asm_tree
open Compiler.Parser

module Main = struct
  let text = "var a := 10; if a < 20 then a += 2; else a -= 2; endif a:= 15;"
  let cur_stack_pointer = ref 16
  let input = parse_program text
  let fin = asm_tree input cur_stack_pointer
  let () = print_endline ""
end
