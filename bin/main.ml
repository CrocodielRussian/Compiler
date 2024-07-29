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
  def main() {
    var a := 0;
    while(a < 10) do
      if(a > 5) then
        break
      endif
      a += 1;
    done
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
