module Main = struct
  let text = "a:=10+11+10;"
  let pos = ref 0
  let res = Compiler.Parser.asign_stmt text pos
  let () = Compiler.Parser.stmt_to_string text pos res |> print_endline
end
