module Main = struct
  let text = "while a < 10 do a += 1; done var a := -(b += c *= 10 + b) < 90;"
  let pos = ref 0
  let res = Compiler.Parser.parse_program text pos
  let () = Compiler.Parser.string_of_statements text pos res |> print_endline
end
