open Compiler.Riscv_translator

module Main = struct
  let text =
    "var a := 10; var b := 20; var c := 20; var d := 20; var e := 20; if a > \
     20 then a += 1; else a += 10; endif"

  let () = asm_translator text
end
