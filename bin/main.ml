open Compiler.Parser
open Compiler.Optimizator
open Compiler.Asm_tree
open Compiler.Riscv_translator

module Main = struct
  let input_file = Sys.argv.(1)
  let output_file = Sys.argv.(2)

  let read_program_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true do
        lines := input_line chan :: !lines
      done;
      !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines

  let append_to_file filename content =
    let oc = open_out filename in
    output_string oc content;
    close_out oc

  let text = read_program_file input_file |> String.concat "\n"
  let try_read in_filename = read_program_file in_filename

  let compile out_filename =
    let content =
      parse_program text |> optimize_ast |> program_to_asm_tree
      |> string_of_instr_list
    in
    append_to_file out_filename content
    

  let test_ast_lang out_filename =
    let all = ref "" in
    List.iter(fun str -> (all := !all ^ (string_of_structure str) ^ "\n")) (parse_program text |> optimize_ast);
    append_to_file out_filename !all
  
  let test_ast_asm out_filename =
    let all = ref "" in
    List.iter(fun str -> (all := !all ^ (show_instr str))) (parse_program text |> optimize_ast |> program_to_asm_tree);
    append_to_file out_filename !all

  let () = 
  match Sys.argv.(3) with 
  | "--compile" -> compile output_file
  | "--ast_lang" -> test_ast_lang output_file
  | "--ast_asm" -> test_ast_asm output_file
  | _ -> failwith "Don't exist"
  end
