module Main = struct
  let text = "var a := -(b := 5) < 90;"
  let pos = ref 0
  let res = Compiler.Parser.parse_program text pos
  let () = Compiler.Parser.string_of_statements text pos res |> print_endline
end
