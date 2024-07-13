module Main = struct
  let text =
    "if n then a:= 1230; b:=20; c:= 10; else a:= 20; b:= 10; c:= 20; endif"

  let pos = ref 0
  let res = Compiler.Parser.global_statements text pos
  let () = Compiler.Parser.stmts_to_string text pos res |> print_endline
end

let expected =
  "If <var: n> then\n\
   <var: a> := 1230;\n\
   <var: b> := 20;\n\
   <var: c> := 10; \n\
   else\n\
   <var: a> := 20;\n\
   <var: b> := 10;\n\
   <var: c> := 20; \n\
   endif\n"
