open Compiler.Parser
open Compiler.Optimizator
open Compiler.Asm_tree
open Compiler.Riscv_translator

module Main = struct
  let append_to_file filename content =
    let oc = open_out filename in
    output_string oc content;
    close_out oc

  let text =
    {|
    def medisum(a, b) {
      return (a + b) / 2;
    }

    def main() {
      23+-12;
      var a := 10 + -0;
      a := medisum(read_int() + (a := read_int()), -a + read_int()) + read_int();
      var b := 10 + print_int(a);
      print_int(a + b * (2 + 9));
      b + -a + read_int();

      if read_int() > 10 then print_int(0); else print_int(1); endif
      while read_int() > 10 do <stmts> done
      
      while true do if (read_int() > 10) then <stmts> else break endif
      return 0 + 10;
    }
    |}

  let compile out_filename =
    let content =
      parse_program text |> optimize_ast |> program_to_asm_tree
      |> string_of_instr_list
    in
    append_to_file out_filename content

  let () = compile "lang/main.s"
end
