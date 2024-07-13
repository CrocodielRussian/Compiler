module Main = struct
  let text = "while a do a:=0; b:=0; c:=10; done"
  let pos = ref 0
  let res = Compiler.Parser.global_statements text pos
  let () = Compiler.Parser.stmts_to_string text pos res |> print_endline
end
