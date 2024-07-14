module Main = struct
  let text = "var a := 10; a + b;10 + a * b; a:=5;"
  let pos = ref 0
  let res = Compiler.Parser.parse_program text pos
  let () = Compiler.Parser.string_of_statements text pos res |> print_endline
end
