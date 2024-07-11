include Compiler.Parser.Parser

module Main = struct
  let text = "10+11+10"
  let pos = ref 0
  let res = Compiler.Parser.Parser.parse_expr text pos
  let () = Compiler.Parser.Parser.expr_to_string text pos res |> print_endline
end
