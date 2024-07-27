open Compiler.Asm_tree
open Compiler.Parser
open Compiler.Riscv_translator

module Main = struct
  let pos = ref 0

  let text =
    {|
  def sum(a,b,c) {
    print_int(a);
    print_int(b);
    print_int(c);
    return a + b + c;
  }

  def sum(a,b,c) {
    print_int(a);
    print_int(b);
    print_int(c);
    return a + b + c;
  }
  
  def main() {
    var b := sum(10 + 20, 90 - 30, -0); 
    print_int(b);
    return 0;
  }
  |}

  let () =
    let program = parse_program text in
    (* List.iter (fun st -> show_structure st |> print_endline) program; *)
    let instructions = program_to_asm_tree program in
    (* List.iter
       (fun instruction -> print_endline (show_instr instruction))
       instructions; *)
    print_endline (string_of_instr_list instructions)
end
