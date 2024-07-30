open Compiler.Parser

(* open Compiler.Optimizator *)
open Compiler.Asm_tree
open Compiler.Riscv_translator

module Main = struct
  let append_to_file filename content =
    let oc = open_out filename in
    output_string oc content;
    close_out oc
  let text =
    {|
  def read_int() {
    var acc := 0;
    var is_negative := 0;
    var code := read_char();
    if code == 45 || code == 43 then
      is_negative := code == 45;
      code := read_char() - 48;
    else
      code := code - 48;
    endif
    while 0 <= code && code <= 9 do
      acc := acc * 10 + code;
      code := read_char() - 48;
    done
    if is_negative then
      acc *= -1;
    endif
    return acc;
  }

  def main() {
    var a := read_int();
    var b := read_int();
    print_int(a + b);
    return 0;
  }

  |}

  let compile out_filename =
    let content =
      parse_program text |> program_to_asm_tree |> string_of_instr_list
    in
    append_to_file out_filename content

  let () = compile "lang/main.s"
end
