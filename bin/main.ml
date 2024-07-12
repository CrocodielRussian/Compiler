module Main = struct
  let text = "if n then a:= 10; a:= 10; else a:=20; endif"
  let pos = ref 0
  let res = Compiler.Parser.global_statements text pos
  let () = Compiler.Parser.stmts_to_string text pos res |> print_endline
end
