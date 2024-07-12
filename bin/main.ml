module Main = struct
  let text = "a:=10+11+10;"
  let pos = ref 0
  let res = Compiler.StatementParser.stmt text pos

  let () =
    Compiler.Parser.StatementParser.stmt_to_string text pos res |> print_endline
end
