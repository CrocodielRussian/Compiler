open Compiler.Parser
open Compiler.Optimizator
open Compiler.Asm_tree
open Compiler.Riscv_translator

module Main = struct
  (* module Unix = UnixLabels *)
  let input_file = Sys.argv.(1)
  let output_file = Sys.argv.(2)

  let read_program_file filename  = 
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;

  let append_to_file filename content =
    let oc = open_out filename in
    output_string oc content;
    close_out oc
  let text = read_program_file input_file |> (String.concat "\n" )
  let try_read in_filename = 
    read_program_file in_filename
  let compile out_filename =
    let content =
      parse_program text |> optimize_ast |> program_to_asm_tree
      |> string_of_instr_list
    in
    append_to_file out_filename content
  
  let test_ast_lang out_filename =
    let content =
      parse_program text |> optimize_ast |> program_to_asm_tree
      |> string_of_instr_list
    in
    append_to_file out_filename content
  
  let test_ast_asm out_filename =
    let content =
      parse_program text |> optimize_ast |> program_to_asm_tree
      |> string_of_instr_list
    in
    append_to_file out_filename content


  let () = 
  match Sys.argv.(3) with 
  | "--compile" -> compile output_file
  | "--ast_lang" -> (
    let program = parse_program text in
    List.iter (fun st -> show_structure st |> print_endline) program;)
  | "--ast_asm" -> (
    let program = parse_program text in
    let instructions = program_to_asm_tree program in
    List.iter(fun instruction -> print_endline (show_instr instruction)) instructions;)
  | _ -> failwith "Don't exist"
  end