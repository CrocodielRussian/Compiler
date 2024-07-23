open Compiler.Riscv_translator

module Main = struct
  let text = "var a:= 12; while a > 0 do a /= 2; done a-=-a;"

  let () =
    let file = "lang/main.s" in
    let asm_code = asm_translator text in
    let oc = open_out file in
    Printf.fprintf oc "%s\n" asm_code;
    close_out oc
end
