module Main = struct
  let text = "while a do a:=0; done"
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
