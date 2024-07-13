open Alcotest

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

let test_if_expression_with_else_and_one_statement text () =
  let pos = ref 0 in
  let greeting =
    Compiler.Parser.global_statements text pos
    |> Compiler.Parser.stmts_to_string text pos
  in
  prerr_endline greeting;
  let expected =
    "If <var: n> then\n<var: a> := 1230;\nelse\n<var: a> := 20;\nendif\n"
  in
  check string "same string" greeting expected

let examples_for_if_expression_with_else_and_one_statement =
  [
    ( "can parse if..else statement with else and one statement",
      `Quick,
      test_if_expression_with_else_and_one_statement
        "if n then a:= 1230; else a:= 20; endif" );
    ( "can parse if..else statement with else and one statement",
      `Quick,
      test_if_expression_with_else_and_one_statement
        "if   n    then\n a:= 1230; else \t a:= 20; endif" );
    ( "can parse if..else statement with else and one statement",
      `Quick,
      test_if_expression_with_else_and_one_statement
        "if n then \ta:= 1230;\n else a:= 20; endif" );
    ( "can parse if..else statement with else and one statement",
      `Quick,
      test_if_expression_with_else_and_one_statement
        "if n then a:= 1230;\n  else a:= 20; endif" );
  ]

let test_if_expression_with_else_and_several_statements text () =
  let pos = ref 0 in
  let greeting =
    Compiler.Parser.global_statements text pos
    |> Compiler.Parser.stmts_to_string text pos
  in
  prerr_endline greeting;
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
  in
  check string "same string" greeting expected

let examples_for_if_expression_with_else_and_several_statements =
  [
    ( "can parse if..else statement with else and several statements",
      `Quick,
      test_if_expression_with_else_and_several_statements
        "if n then a:= 1230; b:=20; c:= 10; else a:= 20; b:= 10; c:= 20; endif"
    );
    ( "can parse if..else statement with else and several statements",
      `Quick,
      test_if_expression_with_else_and_several_statements
        "if n then a:= 1230; b:=20; c:= 10; else       \n\
        \ a:= 20; b:= 10; c:= 20; endif" );
    ( "can parse if..else statement with else and several statements",
      `Quick,
      test_if_expression_with_else_and_several_statements
        "if n then a:= 1230; b:=20; c:= 10; else a:= 20; b:= 10; c:= 20; endif"
    );
    ( "can parse if..else statement with else and several statements",
      `Quick,
      test_if_expression_with_else_and_several_statements
        "if n then \n\
        \ \n\
         \t a:= 1230;\n\
         \t b:=20; \t\n\
         c:= 10; else\n\
        \ \ta:= 20;\n\
        \ \t\n\
         b:= 10; \n\
         c:= 20; endif" );
  ]

let test_if_expression_without_else_and_with_one_statement text () =
  let pos = ref 0 in
  let greeting =
    Compiler.Parser.global_statements text pos
    |> Compiler.Parser.stmts_to_string text pos
  in
  prerr_endline greeting;
  let expected = "If <var: n> then\n<var: a> := 1230;\nelse\n\nendif" in
  check string "same string" greeting expected

let examples_for_if_expression_without_else_and_with_one_statement =
  [
    ( "can parse if..else statement without else and with one statement",
      `Quick,
      test_if_expression_without_else_and_with_one_statement
        "if n then a:= 1230;endif" );
    ( "can parse if..else statement without else and with one statement",
      `Quick,
      test_if_expression_without_else_and_with_one_statement
        "if   n    then\n a:= 1230; endif" );
    ( "can parse if..else statement without else and with one statement",
      `Quick,
      test_if_expression_without_else_and_with_one_statement
        "if n then \ta:= 1230; endif" );
    ( "can parse if..else statement without else and with one statement",
      `Quick,
      test_if_expression_without_else_and_with_one_statement
        "if n then \n a:= 1230;\n  endif" );
  ]

let () =
  Alcotest.run "Tests"
    [
      ("One expression", examples_for_parse_one_expression);
      ("Two expressions", examples_for_parse_two_expression);
      ("Several expressions", examples_for_parse_several_expression);
      ( "If..else statement with else and one statement",
        examples_for_if_expression_with_else_and_one_statement );
      ( "If..else statement with else and several statements",
        examples_for_if_expression_with_else_and_several_statements );
      ( "If..else statement without else and with one statement",
        examples_for_if_expression_without_else_and_with_one_statement );
    ]
