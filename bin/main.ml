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
<<<<<<< HEAD
  def main() {
    var a := 10;
    if(a > 10) then
      print_int(10);  
    endif
=======
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
>>>>>>> 6fbfb6c644e67c1636a9e16dacd582717a5b4280
    return 0;
  }

  |}

  let compile out_filename =
    let content =
      parse_program text |> program_to_asm_tree |> string_of_instr_list
    in
    append_to_file out_filename content

  let () = compile "lang/main.s"
  (* let program = parse_program text in
     List.iter (fun st -> show_structure st |> print_endline) program; *)
     (* let instructions = program_to_asm_tree program in
     List.iter
        (fun instruction -> print_endline (show_instr instruction))
        instructions;
     print_endline (string_of_instr_list instructions) *)

end
