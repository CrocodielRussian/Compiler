open Alcotest
open Compiler.Parser

let test_string_of_ast ast expected_string executor () =
  check string "???" expected_string (executor ast)

let string_of_ast_cases =
  [
    ( "Simple expression: 'a'",
      `Quick,
      test_string_of_ast (Variable "a") "a" (fun ast ->
          string_of_expression "" 0 ast) );
    ( "Simple expression: '10'",
      `Quick,
      test_string_of_ast (Number 10) "10" (fun ast ->
          string_of_expression "" 0 ast) );
    ( "Math expression: '10 + a / (34 * -23 - 90)'",
      `Quick,
      test_string_of_ast
        (Binary
           ( Number 10,
             Plus,
             Binary
               ( Variable "a",
                 Divide,
                 Binary
                   ( Binary (Number 34, Multiply, Unary (Minus, Number 23)),
                     Minus,
                     Number 90 ) ) ))
        "(10 + (a / ((34 * -(23)) - 90)))"
        (fun ast -> string_of_expression "" 0 ast) );
    ( "Compare expression: '10 != a <= (34 * 23 > -90)'",
      `Quick,
      test_string_of_ast
        (Binary
           ( Number 10,
             Unequal,
             Binary
               ( Variable "a",
                 LowOrEqual,
                 Binary
                   ( Binary (Number 34, Multiply, Number 23),
                     More,
                     Unary (Minus, Number 90) ) ) ))
        "(10 != (a <= ((34 * 23) > -(90))))"
        (fun ast -> string_of_expression "" 0 ast) );
    ( "Assign expression: 'a := -(b := 5) < 90'",
      `Quick,
      test_string_of_ast
        (AssignExpression
           ( "a",
             DefaultAssign,
             Binary
               ( Unary (Minus, AssignExpression ("b", DefaultAssign, Number 5)),
                 Low,
                 Number 90 ) ))
        "(a := (-((b := 5)) < 90))"
        (fun ast -> string_of_expression "" 0 ast) );
    ( "Expression statement: '10;'",
      `Quick,
      test_string_of_ast (Expression (Number 10)) "10;" (fun ast ->
          string_of_statement "" 0 ast) );
    ( "Math statement: '10 + a / (34 * -23 - 90);'",
      `Quick,
      test_string_of_ast
        (Expression
           (Binary
              ( Number 10,
                Plus,
                Binary
                  ( Variable "a",
                    Divide,
                    Binary
                      ( Binary (Number 34, Multiply, Unary (Minus, Number 23)),
                        Minus,
                        Number 90 ) ) )))
        "(10 + (a / ((34 * -(23)) - 90)));"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "Compare statement: '10 != a <= (34 * 23 > -90);'",
      `Quick,
      test_string_of_ast
        (Expression
           (Binary
              ( Number 10,
                Unequal,
                Binary
                  ( Variable "a",
                    LowOrEqual,
                    Binary
                      ( Binary (Number 34, Multiply, Number 23),
                        More,
                        Unary (Minus, Number 90) ) ) )))
        "(10 != (a <= ((34 * 23) > -(90))));"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "Assign expression statement: 'a := -(b := 5) < 90;'",
      `Quick,
      test_string_of_ast
        (Expression
           (AssignExpression
              ( "a",
                DefaultAssign,
                Binary
                  ( Unary
                      (Minus, AssignExpression ("b", DefaultAssign, Number 5)),
                    Low,
                    Number 90 ) )))
        "(a := (-((b := 5)) < 90));"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "Variable init statement: 'var a := -(b := 5) < 90;'",
      `Quick,
      test_string_of_ast
        (AssignStatement
           ( "a",
             Binary
               ( Unary (Minus, AssignExpression ("b", DefaultAssign, Number 5)),
                 Low,
                 Number 90 ) ))
        "var a := (-((b := 5)) < 90);"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "While loop statement: 'while a < b do a /= 2; done'",
      `Quick,
      test_string_of_ast
        (While
           ( Binary (Variable "a", Low, Variable "b"),
             [ Expression (AssignExpression ("a", DivideAssign, Number 2)) ] ))
        "while (a < b) do\n(a /= 2);\ndone"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "While loop statement with several statements: 'while a < b do a /= 2; b \
       /= 2; c/=2; done'",
      `Quick,
      test_string_of_ast
        (While
           ( Binary (Variable "a", Low, Variable "b"),
             [
               Expression (AssignExpression ("c", DivideAssign, Number 2));
               Expression (AssignExpression ("b", DivideAssign, Number 2));
               Expression (AssignExpression ("a", DivideAssign, Number 2));
             ] ))
        "while (a < b) do\n(c /= 2);\n(b /= 2);\n(a /= 2);\ndone"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "Big while loop statement: 'while a < b do a /= 2; b:=b-1; done'",
      `Quick,
      test_string_of_ast
        (While
           ( Binary (Variable "a", Low, Variable "b"),
             [
               (* ! This statements contains in list in reversed order *)
               Expression
                 (AssignExpression
                    ("b", DefaultAssign, Binary (Variable "b", Minus, Number 1)));
               Expression (AssignExpression ("a", DivideAssign, Number 2));
             ] ))
        "while (a < b) do\n(b := (b - 1));\n(a /= 2);\ndone"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "if statement: 'if a < b then a /= 2; else b -= a endif'",
      `Quick,
      test_string_of_ast
        (If
           ( Binary (Variable "a", Low, Variable "b"),
             [ Expression (AssignExpression ("a", DivideAssign, Number 2)) ],
             [ Expression (AssignExpression ("b", MinusAssign, Variable "a")) ]
           ))
        "if (a < b) then\n(a /= 2);\nelse\n(b -= a);\nendif"
        (fun ast -> string_of_statement "" 0 ast) );
    ( "if statement without else: 'if a < b then a /= 2; b -= a; endif'",
      `Quick,
      test_string_of_ast
        (If
           ( Binary (Variable "a", Low, Variable "b"),
             [
               Expression (AssignExpression ("b", MinusAssign, Variable "a"));
               Expression (AssignExpression ("a", DivideAssign, Number 2));
             ],
             [ EmptyStatement ] ))
        "if (a < b) then\n(b -= a);\n(a /= 2);\nendif"
        (fun ast -> string_of_statement "" 0 ast) );
  ]

let build_statement text parse_func =
  let pos = ref 0 in
  parse_func text pos

let string_parse_cases =
  [
    ( "Math statement: '10 + 90 / (34 * -23 + 90) < 3;'",
      `Quick,
      let text = "10 + 90 / (34 * -23 + 90) < 3;" in
      let build = build_statement text parse_expr_statement in
      test_string_of_ast build "((10 + (90 / ((34 * -(23)) + 90))) < 3);"
        (fun ast -> string_of_statement text 0 ast) );
    ( "Simple program",
      `Quick,
      let text =
        "\tvar a:= 10  ;var\t b\t   := 15     ;\nb -=      a;\na       /= b;"
      in
      let build =
        build_statement text (fun t p -> parse_statements t p check_program_end)
      in
      test_string_of_ast build
        "var a := 10;\nvar b := 15;\n(b -= a);\n(a /= b);" (fun ast ->
          string_of_statements text 0 ast) );
    ( "Factorial program",
      `Quick,
      let text =
        "\tvar \t\tacc\t:=1\t;var n:=(((6)));while (n)>(1) \n\
         do acc:=acc\r*n\n\n\
         ;\n\
         n:=n-1;\n\n\n\
         done\n\
         \t"
      in
      let build = build_statement text parse_program in
      test_string_of_ast build
        "var acc := 1;\n\
         var n := 6;\n\
         while (n > 1) do\n\
         (acc := (acc * n));\n\
         (n := (n - 1));\n\
         done\n" (fun ast -> string_of_statements text 0 ast) );
    ( "Fibonachi program",
      `Quick,
      let text =
        "var a:=0; var b:=1; var n:=5; while n>1 do b:=a+b;a:=b-a;n:=n-1;done"
      in
      let build = build_statement text parse_program in
      test_string_of_ast build
        "var a := 0;\n\
         var b := 1;\n\
         var n := 5;\n\
         while (n > 1) do\n\
         (b := (a + b));\n\
         (a := (b - a));\n\
         (n := (n - 1));\n\
         done" (fun ast -> string_of_statements text 0 ast) );
  ]

let () =
  Alcotest.run "Parser_tests"
    [
      ("String of AST tests", string_of_ast_cases);
      ("String parse tests", string_parse_cases);
    ]
