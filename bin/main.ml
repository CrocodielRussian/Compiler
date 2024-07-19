open Compiler.Parser

module Main = struct
  module StringMap = Map.Make (String)

  let init_variables (cur_stack_pointer : int ref)
      (statements : statement list ref) =
    let variables_stack_position : int StringMap.t ref = ref StringMap.empty in
    let new_statements_list : statement list ref = ref [] in
    List.iter
      (fun stmt ->
        match stmt with
        | AssignStatement (v, _) ->
            cur_stack_pointer := !cur_stack_pointer + 4;
            variables_stack_position :=
              StringMap.add v !cur_stack_pointer !variables_stack_position
        | _ -> new_statements_list := stmt :: !new_statements_list)
      !statements;
    !variables_stack_position

  let () =
    let text =
      "10 + 12 +14 +15; var a := 10; 12 + 90; 45 + 34; var b := 123;"
    in
    let pos = ref 0 in
    let statements = ref (parse_program text pos) in
    let cur_stack_pointer = ref 16 in
    let variables_stack_position =
      init_variables cur_stack_pointer statements
    in
    StringMap.iter
      (fun key value -> print_endline (key ^ ": " ^ string_of_int value))
      variables_stack_position
  (* TODO: run statements_to_asm *)
end
