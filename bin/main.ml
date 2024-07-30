open Compiler.Parser

(* open Compiler.Optimizator *)
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
      parse_program text |> program_to_asm_tree |> string_of_instr_list
    in
    append_to_file out_filename content


  let () =  compile output_file
  
  (* List.iter(fun str -> print_endline str) (try_read "bin/main.clang" ) *)

  (* let program = parse_program text in
     List.iter (fun st -> show_structure st |> print_endline) program; *)
     (* let instructions = program_to_asm_tree program in
     List.iter
        (fun instruction -> print_endline (show_instr instruction))
        instructions;
     print_endline (string_of_instr_list instructions) *)
  end