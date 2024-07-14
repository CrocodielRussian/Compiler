(* open Alcotest

   let test_one_parse_expression text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     let expected = "<var: num> := 10;" in
     check string "same string" greeting expected

   let examples_for_parse_one_expression =
     [
       ( "can parse one expression",
         `Quick,
         test_one_parse_expression "num:=       10;" );
       ("can parse one expression", `Quick, test_one_parse_expression "num:=10;");
       ( "can parse one expression",
         `Quick,
         test_one_parse_expression "num:=       10;" );
       ( "can parse one expression",
         `Quick,
         test_one_parse_expression "num:=       10      ;" );
     ]

   let test_two_parse_expression text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     prerr_endline greeting;
     let expected = "<var: a> := 10;\n<var: b> := 1230;" in
     check string "same string" greeting expected

   let examples_for_parse_two_expression =
     [
       ( "can parse two expression",
         `Quick,
         test_two_parse_expression "a:=       10      ; b:=   1230;" );
       ( "can parse two expression",
         `Quick,
         test_two_parse_expression "a:=       10      ; b:= 1230;" );
       ( "can parse two expression",
         `Quick,
         test_two_parse_expression "a:=       10      ; b:=1230;" );
       ( "can parse two expression",
         `Quick,
         test_two_parse_expression "a:=       10      ; b:= 1230;" );
     ]

   let test_several_parse_expression text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     prerr_endline greeting;
     let expected = "<var: a> := 10;\n<var: b> := 1230;\n<var: num> := 15;" in
     check string "same string" greeting expected

   let examples_for_parse_several_expression =
     [
       ( "can parse several expression",
         `Quick,
         test_several_parse_expression "a:=       10      ; b:=   1230;num:= 15;"
       );
       ( "can parse several expression",
         `Quick,
         test_several_parse_expression "a:=       10      ; b:= 1230;   num:= 15;"
       );
       ( "can parse several expression",
         `Quick,
         test_several_parse_expression
           "a:=       10      ; b:=1230; \t \t num:= 15;" );
       ( "can parse several expression",
         `Quick,
         test_several_parse_expression "a:=       10      ; b:= 1230; \n num:= 15;"
       );
     ]

   let test_if_statement_with_else_and_one_statement text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     prerr_endline greeting;
     let expected =
       "if <var: n> then\n<var: a> := 1230;\nelse\n<var: a> := 20;\nendif\n"
     in
     check string "same string" greeting expected

   let examples_for_if_statement_with_else_and_one_statement =
     [
       ( "can parse if..else statement with else and one statement",
         `Quick,
         test_if_statement_with_else_and_one_statement
           "if n then a:= 1230; else a:= 20; endif" );
       ( "can parse if..else statement with else and one statement",
         `Quick,
         test_if_statement_with_else_and_one_statement
           "if   n    then\n a:= 1230; else \t a:= 20; endif" );
       ( "can parse if..else statement with else and one statement",
         `Quick,
         test_if_statement_with_else_and_one_statement
           "if n then \ta:= 1230;\n else a:= 20; endif" );
       ( "can parse if..else statement with else and one statement",
         `Quick,
         test_if_statement_with_else_and_one_statement
           "if n then a:= 1230;\n  else a:= 20; endif" );
     ]

   let test_if_statement_with_else_and_several_statements text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     prerr_endline greeting;
     let expected =
       "if <var: n> then\n\
        <var: a> := 1230;\n\
        <var: b> := 20;\n\
        <var: c> := 10;\n\
        else\n\
        <var: a> := 20;\n\
        <var: b> := 10;\n\
        <var: c> := 20;\n\
        endif\n"
     in
     check string "same string" greeting expected

   let examples_for_if_statement_with_else_and_several_statements =
     [
       ( "can parse if..else statement with else and several statements",
         `Quick,
         test_if_statement_with_else_and_several_statements
           "if n then a:= 1230; b:=20; c:= 10; else a:= 20; b:= 10; c:= 20; endif"
       );
       ( "can parse if..else statement with else and several statements",
         `Quick,
         test_if_statement_with_else_and_several_statements
           "if n then a:= 1230; b:=20; c:= 10; else       \n\
           \ a:= 20; b:= 10; c:= 20; endif" );
       ( "can parse if..else statement with else and several statements",
         `Quick,
         test_if_statement_with_else_and_several_statements
           "if n then a:= 1230; b:=20; c:= 10; else a:= 20; b:= 10; c:= 20; endif"
       );
       ( "can parse if..else statement with else and several statements",
         `Quick,
         test_if_statement_with_else_and_several_statements
           "if n then a:= 1230; b:=20; c:= 10; else a:= 20; b:= 10; c:= 20; endif"
       );
     ]

   let test_if_statement_without_else_and_with_one_statement text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     prerr_endline greeting;
     let expected = "if <var: n> then\n<var: a> := 1230;\nelse\n\nendif" in
     check string "same string" greeting expected

   let examples_for_if_statement_without_else_and_with_one_statement =
     [
       ( "can parse if..else statement without else and with one statement",
         `Quick,
         test_if_statement_without_else_and_with_one_statement
           "if n then a:= 1230;endif" );
       ( "can parse if..else statement without else and with one statement",
         `Quick,
         test_if_statement_without_else_and_with_one_statement
           "if   n    then\n a:= 1230; endif" );
       ( "can parse if..else statement without else and with one statement",
         `Quick,
         test_if_statement_without_else_and_with_one_statement
           "if n then \ta:= 1230; endif" );
       ( "can parse if..else statement without else and with one statement",
         `Quick,
         test_if_statement_without_else_and_with_one_statement
           "if n then \n a:= 1230;\n  endif" );
     ]

   let test_while_loop_statement_with_several_statements text () =
     let pos = ref 0 in
     let greeting =
       Compiler.Parser.global_statements text pos
       |> Compiler.Parser.stmts_to_string text pos
     in
     prerr_endline greeting;
     let expected =
       "while <var: a> do\n<var: a> := 0;\n<var: b> := 0;\n<var: c> := 10;\ndone"
     in
     check string "same string" greeting expected

   let examples_for_while_loop_statement_with_several_statements =
     [
       ( "can parse while loop statement with several statements",
         `Quick,
         test_while_loop_statement_with_several_statements
           "while a do\n \n \ta:=0;\n b:=0; \nc:=10; \ndone" );
       ( "can parse while loop statement with several statements",
         `Quick,
         test_while_loop_statement_with_several_statements
           "while a do a:=0; b:=0; c:=10; done" );
       ( "can parse while loop statement with several statements",
         `Quick,
         test_while_loop_statement_with_several_statements
           "while a do\n \n \ta:=0;     \n b:=0;      \nc:=10; \ndone" );
       ( "can parse while loop statement with several statements",
         `Quick,
         test_while_loop_statement_with_several_statements
           "while a do\n \n \ta:=0;\n \t\tb:=0; \t\nc:=10;      done" );
     ]

   let () =
     Alcotest.run "Tests"
       [
         ("One expression", examples_for_parse_one_expression);
         ("Two expressions", examples_for_parse_two_expression);
         ("Several expressions", examples_for_parse_several_expression);
         ( "if..else statement with else and one statement",
           examples_for_if_statement_with_else_and_one_statement );
         ( "if..else statement with else and several statements",
           examples_for_if_statement_with_else_and_several_statements );
         ( "if..else statement without else and with one statement",
           examples_for_if_statement_without_else_and_with_one_statement );
         ( "while loop statement with several statements",
           examples_for_while_loop_statement_with_several_statements );
       ] *)
