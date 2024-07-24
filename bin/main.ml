open Compiler.Riscv_translator

module Main = struct
  let text =
    "var a:= -12 * +10 - 10 / 10 + 10; while a > 10 /  (1 + 0) do a /= (a + \
     -2); done"

  let () =
    let file = "lang/main.s" in
    let asm_code = asm_translator text in
    let oc = open_out file in
    Printf.fprintf oc "%s\n" asm_code;
    close_out oc
end
