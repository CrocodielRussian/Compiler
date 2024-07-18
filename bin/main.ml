module Main = struct
  let text =
    "var acc := 1;                      \n\
     var n := 6;\n\
     while (n > 1) do\n\
     (acc := (acc * n));\n\
     (n := (n - 1));\n\
     done\n\
     var a := 0;\n\
     var b := 1;\n\
     (n := 5);\n\
     while (n > 1) do\n\
     (b := (a + b));\n\
     (a := (b - a));\n\
     (n := (n - 1));\n\
     done\n"

  let pos = ref 0
  let res = Compiler.Parser.parse_program text pos
  let () = Compiler.Parser.string_of_statements text pos res |> print_endline
end
