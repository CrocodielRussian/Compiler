  $  dune exec compiler -- simple_variable.clang /dev/null --ast_lang
  (Parser.FuncStruct ("main", [],     
   [(Parser.AssignStatement ("a", (Parser.Number 10)));
     (Parser.ReturnStatement (Parser.Number 0))]
   ))
  

