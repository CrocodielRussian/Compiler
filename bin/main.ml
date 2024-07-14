module Main = struct
  let text =
    "while a + b * c >= 0 do a:=(a + b * c) != 12; b:=13 + 65 * a < 12; c:= a \
     < b; done"

  let pos = ref 0
  let res = Compiler.Parser.global_statements text pos
  let () = Compiler.Parser.stmts_to_string text pos res |> print_endline
end
