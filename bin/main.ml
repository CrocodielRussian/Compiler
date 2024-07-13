module Main = struct
  let text = "a:= 10+20+30*10*15+11;"
  let pos = ref 0
  let res = Compiler.Parser.global_statements text pos
  let () = Compiler.Parser.stmts_to_string text pos res |> print_endline
end
