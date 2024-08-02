open Exceptions
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type oper =
  | Plus
  | Multiply
  | Divide
  | Mod
  | Minus
  | Low
  | More
  | LowOrEqual
  | MoreOrEqual
  | Equal
  | Unequal
  | AndOper
  | OrOper
  | NotOper
  | Invalid
[@@deriving show]

(* Assotiate with: := | += | *= | /= | -= | Invalid *)
type assign_oper =
  | DefaultAssign
  | PlusAssign
  | MultiplyAssign
  | DivideAssign
  | MinusAssign
  | InvalidAssing
[@@deriving show]

type expr =
  | Variable of string
  | Number of int (* 12 *)
  | Unary of oper * expr (* -12 *)
  | Binary of expr * oper * expr (* 14  + 12 *)
  | AssignExpression of string * assign_oper * expr (* a := 10*)
  | FuncCall of string * expr list (* func(a, b, a + b) *)
  | EmptyExpression
[@@deriving show]

type statement =
  | Expression of expr
  | ReturnStatement of expr
  | BreakStatement
  | AssignStatement of string * expr (* a := 10 + 20;*)
  | EmptyStatement
  | While of expr * statement list
  | If of expr * statement list * statement list
[@@deriving show]

type structure = FuncStruct of string * string list * statement list
[@@deriving show]

let string_of_unary_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | NotOper -> "!"
  | Invalid -> throw_except (ASTError "unexpected operator")
  | _ -> throw_except (ASTError "unexpected unary operator")
[@@deriving show]

let string_of_binary_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Low -> "<"
  | LowOrEqual -> "<="
  | More -> ">"
  | MoreOrEqual -> ">="
  | Equal -> "=="
  | Unequal -> "!="
  | AndOper -> "&&"
  | OrOper -> "||"
  | Mod -> "%"
  | _ -> throw_except (ASTError "unexpected binary operator")
[@@deriving show]

let string_of_assign_operator = function
  | DefaultAssign -> ":="
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | MultiplyAssign -> "*="
  | DivideAssign -> "/="
  | InvalidAssing -> "?=?"
[@@deriving show]

let rec string_of_expression = function
  | Variable n -> n
  | Number n -> string_of_int n
  | Unary (op, e) ->
      string_of_unary_operator op ^ "(" ^ string_of_expression e ^ ")"
  | Binary (e1, op, e2) ->
      "(" ^ string_of_expression e1 ^ " "
      ^ string_of_binary_operator op
      ^ " " ^ string_of_expression e2 ^ ")"
  | AssignExpression (v, op, e) ->
      "(" ^ v ^ " "
      ^ string_of_assign_operator op
      ^ " " ^ string_of_expression e ^ ")"
  | FuncCall (n, expressions) ->
      let expressions_string = ref [] in
      List.iter
        (fun ex ->
          expressions_string :=
            !expressions_string @ [ string_of_expression ex ])
        expressions;
      Printf.sprintf "%s(%s)" n (String.concat ", " !expressions_string)
  | EmptyExpression -> ""

let rec string_of_statements stmts count_of_t =
  let str_lst =
    List.map (fun stmt -> string_of_statement stmt count_of_t) stmts
  in
  String.concat "\n" str_lst

and string_of_statement stmt count_of_t =
  match stmt with
  | EmptyStatement -> ""
  | BreakStatement -> String.make count_of_t '\t' ^ "break;"
  | ReturnStatement e ->
      String.make count_of_t '\t'
      ^ Printf.sprintf "return %s;" (string_of_expression e)
  | Expression e ->
      String.make count_of_t '\t'
      ^ Printf.sprintf "%s;" (string_of_expression e)
  | AssignStatement (v, e2) ->
      String.make count_of_t '\t'
      ^ Printf.sprintf "var %s := %s;" v (string_of_expression e2)
  | While (e1, stmts) ->
      String.make count_of_t '\t'
      ^ "while " ^ string_of_expression e1 ^ " do\n"
      ^ string_of_statements stmts (count_of_t + 1)
      ^ "\n"
      ^ String.make count_of_t '\t'
      ^ "done"
  | If (e1, if_stmts, else_stmts) ->
      if else_stmts = [ EmptyStatement ] then
        String.make count_of_t '\t'
        ^ "if " ^ string_of_expression e1 ^ " then\n"
        ^ string_of_statements if_stmts (count_of_t + 1)
        ^ "\n"
        ^ String.make count_of_t '\t'
        ^ "endif"
      else
        String.make count_of_t '\t'
        ^ "if " ^ string_of_expression e1 ^ " then\n"
        ^ string_of_statements if_stmts (count_of_t + 1)
        ^ "\n"
        ^ String.make count_of_t '\t'
        ^ "else\n"
        ^ string_of_statements else_stmts (count_of_t + 1)
        ^ "\n"
        ^ String.make count_of_t '\t'
        ^ "endif"

and string_of_structure = function
  | FuncStruct (name, arg_var, stmts) ->
      let count_of_t = 1 in
      Printf.sprintf "def %s(%s){\n%s\n}" name
        (String.concat ", " arg_var)
        (string_of_statements stmts count_of_t)

let initialised_functions =
  ref
    (StringSet.of_list
       [ "_start"; "put_char"; "get_char"; "read_int"; "print_int" ])

let functions_args_count : int StringMap.t ref =
  ref
    StringMap.(
      empty |> add "put_char" 1 |> add "get_char" 0 |> add "read_int" 0
      |> add "print_int" 1)

let count_of_newline = ref 0
let cur_pos_on_line = ref 0
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' | '\r' | '\t' | '\n' -> true | _ -> false
let is_newline = function '\n' -> true | _ -> false

let expect_symbol text pos symbol =
  if pos >= 0 && pos < String.length text then text.[pos] == symbol else false

(**
   Skips any whitespace characters in the given string starting from the specified position.
   It also updates the line count and current position on the line accordingly.

   [text] The string to be processed.
   [pos] The mutable reference to the current position in the string.
*)
let skip_whitespaces text pos =
  let length = String.length text in
  while !pos < length && is_whitespace text.[!pos] do
    if is_newline text.[!pos] then (
      incr count_of_newline;
      cur_pos_on_line := 0)
    else ();
    incr pos;
    incr cur_pos_on_line
  done

(**
   Parses a positive number from a given string.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.

   Returns:
   - [Number n]: If the positive number is successfully parsed, it returns a [Number] record with the integer value of the number.
   - [ParserError]: If the positive number cannot be parsed, it throws a [ParserError] exception.

   The function skips any whitespace characters, initializes an accumulator to store the digits of the number, and iterates through the characters of the string until it encounters a non-digit character. It then converts the accumulated digits into an integer and returns a [Number] record with the value. If no digits are found, it throws a [ParserError] exception.
*)
let positive_number text pos =
  skip_whitespaces text pos;
  let acc = ref "" in
  let length = String.length text in
  while !pos < length && is_digit text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos;
    incr cur_pos_on_line
  done;
  if String.length !acc > 0 then Number (int_of_string !acc)
  else
    throw_except
      (ParserError (!count_of_newline, !cur_pos_on_line, "couldn't find numbers"))

(**
   Parses an identifier from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

   The function creates a reference to hold the parsed identifier and enters a loop that continues until the current position is past the end of the string or the current character is not an alphabetic character or an underscore.
   In each iteration, it appends the current character to the identifier reference and increments the position.
   After the loop, it returns the final parsed identifier.
*)
let identifier text pos =
  let acc = ref "" in
  while
    !pos < String.length text && (is_alpha text.[!pos] || '_' = text.[!pos])
  do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos;
    incr cur_pos_on_line
  done;
  !acc

(**
   Parses a variable from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   The function first skips any whitespace characters.
   It then extracts an identifier from the string starting at the current position.
   If the identifier is a valid variable (i.e., it is not empty and has been initialized),
   it returns a [Variable] expression with the identifier.
   If the identifier is not a valid variable, it prints an error message and raises a [LogicErrorParsing] exception.
   If the identifier is empty, it raises a [ParserError] exception.
*)
let variable text pos initialised_variables =
  skip_whitespaces text pos;
  let acc = identifier text pos in
  if String.length acc > 0 then
    if StringSet.mem acc !initialised_variables then Variable acc
    else (
      prerr_endline "<";
      throw_except
        (LogicErrorParsing
           ( !count_of_newline,
             !cur_pos_on_line,
             "find undound variable: '" ^ acc ^ "'" )))
  else
    throw_except
      (ParserError
         (!count_of_newline, !cur_pos_on_line, "couldn't find variable"))

(**
   Checks if a semicolon exists at the current position in the given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

   The function first skips any whitespace characters.
   It then checks if the current position is within the bounds of the string.
   If it is not, it returns false.
   If it is, it matches the character at the current position with a semicolon.
   If the character is a semicolon, it increments the position and returns true.
   If the character is not a semicolon, it returns false.
*)
let check_exists_simple_stmt_close text pos =
  skip_whitespaces text pos;
  if !pos < String.length text then
    match text.[!pos] with
    | ';' ->
        incr pos;
        incr cur_pos_on_line;
        true
    | _ -> false
  else false

(**
   [check_expr_stmt_close] checks if the given position in the text represents the closing symbol of an expression statement.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.
   - [expression]: The expression to be included in the expression statement.

   Returns:
   - [Expression expression]: If the closing symbol is found, it returns an [Expression] record with the given expression.
   - [ParserError]: If the closing symbol is not found, it throws a [ParserError] exception.
*)
let check_expr_stmt_close text pos expression =
  skip_whitespaces text pos;
  if check_exists_simple_stmt_close text pos then Expression expression
  else
    throw_except
      (ParserError
         ( !count_of_newline,
           !cur_pos_on_line,
           "couldn't find close symbol of expression statement: ';'" ))

(**
   Parses an assignment operation from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

   The function first skips any whitespace characters.
   It then checks if the current position plus two is within the bounds of the string.
   If it is not, it returns [InvalidAssing].
   Otherwise, it matches the substring from the current position to the current position plus two with the assignment operators.
   If a match is found, it updates the position and returns the corresponding [assign_oper] value.
   If no match is found, it returns [InvalidAssing].
*)
let parse_assign_operation text pos =
  skip_whitespaces text pos;
  if !pos + 2 > String.length text then InvalidAssing
  else
    match String.sub text !pos 2 with
    | ":=" ->
        pos := !pos + 2;
        DefaultAssign
    | "+=" ->
        pos := !pos + 2;
        PlusAssign
    | "-=" ->
        pos := !pos + 2;
        MinusAssign
    | "/=" ->
        pos := !pos + 2;
        DivideAssign
    | "*=" ->
        pos := !pos + 2;
        MultiplyAssign
    | _ -> InvalidAssing

(**
   Parses a compare operation from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

   The function first skips any whitespace characters.
   It then checks if the current position is within the bounds of the string.
   If it is not, it returns [Invalid].
   Otherwise, it extracts the symbol at the current position.
   If the symbol is '>' or '<', it checks if the next character is '='.
   If it is, it returns the appropriate [oper] value.
   If it isn't, it returns the appropriate [oper] value.
   If the symbol is '!' or '=', it checks if the next character is '='.
   If it is, it returns the appropriate [oper] value.
   If it isn't, it throws a [ParserError] exception.
   If the symbol doesn't match any of the above cases, it returns [Invalid].
*)
let parse_compare_operation text pos =
  skip_whitespaces text pos;
  if !pos < 0 || !pos >= String.length text then Invalid
  else
    let symbol = text.[!pos] in
    match symbol with
    | '>' | '<' -> (
        if expect_symbol text (!pos + 1) '=' then
          match symbol with
          | '<' ->
              pos := !pos + 2;
              LowOrEqual
          | '>' ->
              pos := !pos + 2;
              MoreOrEqual
          | _ -> Invalid
        else
          match symbol with
          | '<' ->
              incr pos;
              incr cur_pos_on_line;
              Low
          | '>' ->
              incr pos;
              incr cur_pos_on_line;
              More
          | _ -> Invalid)
    | '!' | '=' ->
        if expect_symbol text (!pos + 1) '=' then
          match symbol with
          | '!' ->
              pos := !pos + 2;
              Unequal
          | '=' ->
              pos := !pos + 2;
              Equal
          | _ -> Invalid
        else
          throw_except
            (ParserError
               ( !count_of_newline,
                 !cur_pos_on_line,
                 "find unexpected compare operator" ))
    | _ -> Invalid

(**
   Parses an adding operation from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

   The function first skips any whitespace characters.
   It then checks if the current position is within the bounds of the string.
   If it is not, it returns [Invalid].
   Otherwise, it extracts the symbol at the current position.
   If the symbol is '+' or '-', it checks if the next character is '='.
   If it is, it returns [Invalid].
   If it isn't, it increments the position and returns the appropriate [oper] value.
   If the symbol is neither '+' nor '-', it returns [Invalid].
*)
let parse_adding_operation text pos =
  skip_whitespaces text pos;
  if !pos < 0 || !pos >= String.length text then Invalid
  else
    let symbol = text.[!pos] in
    match symbol with
    | '+' | '-' -> (
        if expect_symbol text (!pos + 1) '=' then Invalid
        else
          match symbol with
          | '+' ->
              incr pos;
              incr cur_pos_on_line;
              Plus
          | '-' ->
              incr pos;
              incr cur_pos_on_line;
              Minus
          | _ -> Invalid)
    | _ -> Invalid

(**
   Parses a multiplication or division operation from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

   The function first skips any whitespace characters.
   It then checks if the current position is within the bounds of the string.
   If it is not, it returns [Invalid].
   Otherwise, it extracts the symbol at the current position.
   If the symbol is '*' or '/', it checks if the next character is '='.
   If it is, it returns [Invalid].
   If it isn't, it increments the position and returns the appropriate [oper] value.
   If the symbol is neither '*' nor '/', it returns [Invalid].
*)
let parse_multiply_operation text pos =
  skip_whitespaces text pos;
  if !pos < 0 || !pos >= String.length text then Invalid
  else
    let symbol = text.[!pos] in
    match symbol with
    | '*' | '/' -> (
        if expect_symbol text (!pos + 1) '=' then Invalid
        else
          match symbol with
          | '*' ->
              incr pos;
              incr cur_pos_on_line;
              Multiply
          | '/' ->
              incr pos;
              incr cur_pos_on_line;
              Divide
          | _ -> Invalid)
    | '%' ->
        incr pos;
        Mod
    | _ -> Invalid

let parse_bool_operation text pos =
  skip_whitespaces text pos;
  if !pos + 2 > String.length text then Invalid
  else
    match String.sub text !pos 2 with
    | "&&" ->
        pos := !pos + 2;
        AndOper
    | "||" ->
        pos := !pos + 2;
        OrOper
    | _ -> Invalid

(**
   Parses an expression from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   The function first skips any whitespace characters.
   It then creates a reference to hold the parsed expression and another reference to hold the parsed operation.
   It enters a loop that continues until the current position is past the end of the string or the parsed operation is invalid.
   In each iteration, it updates the expression reference to hold an assignment expression with the current expression, operation, and the next simplest expression.
   If the parsed expression is not a variable, it resets the position to the start position and parses a boolean expression instead.
   After the loop, it returns the final parsed expression.
*)
let rec parse_expr (text : string) (pos : int ref)
    (initialised_variables : StringSet.t ref) : expr =
  skip_whitespaces text pos;
  let start_pos = !pos in
  let expression = parse_math_expr text pos initialised_variables in
  match expression with
  | Variable v -> (
      let operation = parse_assign_operation text pos in
      match operation with
      | InvalidAssing ->
          pos := start_pos;
          parse_bool_expr text pos initialised_variables
      | _ ->
          AssignExpression
            (v, operation, parse_expr text pos initialised_variables))
  | _ ->
      pos := start_pos;
      parse_bool_expr text pos initialised_variables

(**
   Parses a boolean expression from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Returns the final parsed expression.
*)
and parse_bool_expr text pos initialised_variables =
  skip_whitespaces text pos;
  let expression = ref (parse_compare_expr text pos initialised_variables) in
  let operation = ref (parse_bool_operation text pos) in
  while !pos < String.length text && !operation != Invalid do
    expression :=
      Binary
        ( !expression,
          !operation,
          parse_compare_expr text pos initialised_variables );
    operation := parse_bool_operation text pos
  done;
  !expression

(**
   Parses a comparison expression from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Returns the final parsed expression.
*)
and parse_compare_expr text pos initialised_variables =
  skip_whitespaces text pos;
  let expression = ref (parse_math_expr text pos initialised_variables) in
  let operation = ref (parse_compare_operation text pos) in
  while !pos < String.length text && !operation != Invalid do
    expression :=
      Binary
        (!expression, !operation, parse_math_expr text pos initialised_variables);
    operation := parse_compare_operation text pos
  done;
  !expression

(**
   Parses a mathematical expression from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Returns the final parsed expression.
*)
and parse_math_expr text pos initialised_variables =
  skip_whitespaces text pos;
  let expression = ref (parse_mult_expr text pos initialised_variables) in
  let operation = ref (parse_adding_operation text pos) in
  while !pos < String.length text && !operation != Invalid do
    expression :=
      Binary
        (!expression, !operation, parse_mult_expr text pos initialised_variables);
    operation := parse_adding_operation text pos
  done;
  !expression

(**
   Parses a multiplication or division expression from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Returns the final parsed expression.
*)
and parse_mult_expr text pos initialised_variables =
  skip_whitespaces text pos;
  let expression = ref (parse_simplest_expr text pos initialised_variables) in
  let operation = ref (parse_multiply_operation text pos) in
  while !pos < String.length text && !operation != Invalid do
    expression :=
      Binary
        ( !expression,
          !operation,
          parse_simplest_expr text pos initialised_variables );
    operation := parse_multiply_operation text pos
  done;
  !expression

(**
   Parses the simplest expression from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Return result expression
*)
and parse_simplest_expr text pos initialised_variables =
  skip_whitespaces text pos;
  if !pos >= String.length text then EmptyExpression
  else
    match text.[!pos] with
    | '-' ->
        incr pos;
        incr cur_pos_on_line;
        Unary (Minus, parse_simplest_expr text pos initialised_variables)
    | '+' ->
        incr pos;
        incr cur_pos_on_line;
        parse_simplest_expr text pos initialised_variables
    | '!' ->
        incr pos;
        incr cur_pos_on_line;
        Unary (NotOper, parse_simplest_expr text pos initialised_variables)
    | '(' ->
        incr pos;
        incr cur_pos_on_line;
        let result_of_searching = parse_expr text pos initialised_variables in
        skip_whitespaces text pos;
        if !pos < String.length text && text.[!pos] = ')' then (
          incr pos;
          incr cur_pos_on_line;
          result_of_searching)
        else
          throw_except
            (ParserError
               ( !count_of_newline,
                 !cur_pos_on_line,
                 "couldn't find symbol of closed bracket: ')'" ))
    | '0' .. '9' -> positive_number text pos
    | 'a' .. 'z' | 'A' .. 'Z' ->
        let ident = identifier text pos in
        if StringSet.mem ident !initialised_functions then
          parse_func_call ident text pos initialised_variables
        else (
          pos := !pos - String.length ident;
          variable text pos initialised_variables)
    | _ ->
        throw_except
          (ParserError
             ( !count_of_newline,
               !cur_pos_on_line,
               "unexpected symbol: '" ^ Char.escaped text.[!pos] ^ "'" ))

(**
   Parses a function call from a given string.

   [ident] is the identifier of the function being called.
   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Return creates a [FuncCall] record.
*)
and parse_func_call ident text pos initialised_variables =
  skip_whitespaces text pos;
  let symbol = text.[!pos] in
  match symbol with
  | '(' -> (
      incr pos;
      incr cur_pos_on_line;
      skip_whitespaces text pos;
      let func_args_expr : expr list ref = ref [] in
      let expected_args_count =
        ref (StringMap.find ident !functions_args_count)
      in
      while text.[!pos] != ')' && !expected_args_count >= 0 do
        let arg = parse_expr text pos initialised_variables in
        match arg with
        | EmptyExpression ->
            throw_except
              (ParserError
                 (!count_of_newline, !cur_pos_on_line, "couldn't find argument"))
        | _ ->
            func_args_expr := !func_args_expr @ [ arg ];
            decr expected_args_count;
            skip_whitespaces text pos;
            if expect_symbol text !pos ',' || expect_symbol text !pos ')' then
              if expect_symbol text !pos ',' then (
                incr pos;
                incr cur_pos_on_line)
              else ()
            else
              throw_except
                (ParserError
                   ( !count_of_newline,
                     !cur_pos_on_line,
                     "couldn't find symbol of closed bracket for close \
                      function '" ^ ident ^ "' call: ')' or ','" ))
      done;
      if !expected_args_count != 0 then
        throw_except
          (ParserError
             ( !count_of_newline,
               !cur_pos_on_line,
               "on function " ^ ident ^ " call find "
               ^ string_of_int
                   (StringMap.find ident !functions_args_count
                   - !expected_args_count)
               ^ " arguments, but expected argument count is "
               ^ string_of_int (StringMap.find ident !functions_args_count) ))
      else ();
      skip_whitespaces text pos;
      match expect_symbol text !pos ')' with
      | true ->
          incr pos;
          incr cur_pos_on_line;
          FuncCall (ident, !func_args_expr)
      | false ->
          throw_except
            (ParserError
               ( !count_of_newline,
                 !cur_pos_on_line,
                 "couldn't find symbol of closed bracket for close function '"
                 ^ ident ^ "' call: ')'" )))
  | _ ->
      throw_except
        (ParserError
           ( !count_of_newline,
             !cur_pos_on_line,
             "couldn't find symbol of open bracket of function call: '('. Find \
              symbol: '" ^ Char.escaped symbol ))

(**
   Parses an expression statement from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Return finally expression statement
*)
let parse_expr_statement text pos initialised_variables =
  skip_whitespaces text pos;
  let result = parse_expr text pos initialised_variables in
  match result with
  | EmptyExpression -> EmptyStatement
  | _ -> check_expr_stmt_close text pos result

(**
   Parses an assignment statement from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.
*)
let assert_assign_statement_op text pos =
  skip_whitespaces text pos;
  match String.sub text !pos 2 with
  | ":=" -> pos := !pos + 2
  | _ ->
      throw_except
        (ParserError
           ( !count_of_newline,
             !cur_pos_on_line,
             "couldn't find assign operator. Find " ^ String.sub text !pos 2 ))

(**
   Parses an assignment statement from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.
   Return [statement]
*)
let parser_assign_statement text pos initialised_variables =
  skip_whitespaces text pos;
  let ident = identifier text pos in
  assert_assign_statement_op text pos;
  if StringSet.mem ident !initialised_variables then
    throw_except
      (LogicErrorParsing
         ( !count_of_newline,
           !cur_pos_on_line,
           "init variable " ^ ident ^ ", which exists now" ))
  else
    let assign =
      AssignStatement (ident, parse_expr text pos initialised_variables)
    in
    if check_exists_simple_stmt_close text pos then (
      initialised_variables := StringSet.add ident !initialised_variables;
      assign)
    else
      throw_except
        (ParserError
           ( !count_of_newline,
             !cur_pos_on_line,
             "couldn't find close symbol of statement: ';'" ))

(**
   [check_do_exists] checks if the current position in the string represents the existence of a "do" construction.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.

   Returns:
   - [true] if find do else [false].
*)
let check_do_exists text pos =
  skip_whitespaces text pos;
  if !pos + 2 < String.length text then
    match String.sub text !pos 2 with
    | "do" ->
        pos := !pos + 2;
        true
    | _ -> false
  else
    throw_except
      (ParserError (!count_of_newline, !cur_pos_on_line, "couldn't find do"))

(**
   Checks if the current position in the string represents the existence of a "done" construction.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.

   Returns:
   - [true] if not find done else [false].
*)
let check_done_exists text pos =
  skip_whitespaces text pos;
  if !pos + 4 < String.length text then
    match String.sub text !pos 4 with "done" -> false | _ -> true
  else false

(**
   Checks if the current position in the string represents the existence of a "then" construction.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.

   Returns:
   - [true] if then exists else [false].
*)
let check_then_exists text pos =
  skip_whitespaces text pos;
  if !pos + 4 < String.length text then
    match String.sub text !pos 4 with
    | "then" ->
        pos := !pos + 4;
        true
    | _ -> false
  else
    throw_except
      (ParserError (!count_of_newline, !cur_pos_on_line, "couldn't find then"))

(**
   Checks if the current position in the string represents the existence of an "else" or "endif" construction.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.

   Returns:
   - A boolean value indicating whether the current position represents the existence of an "else" or "endif" construction.
     The function returns true if the current position is less than the length of the string and the substring starting from the current position matches either "else " or "endif".
     Otherwise, it returns false.
*)
let check_else_and_endif_construction_exists text pos =
  skip_whitespaces text pos;
  if !pos + 5 < String.length text then
    match String.sub text !pos 5 with
    | "else " | "else\t" | "else\n" | "else\r" -> false
    | "endif" -> false
    | _ -> true
  else false

let check_endif_exists_and_skip text pos =
  skip_whitespaces text pos;
  if !pos + 5 <= String.length text then
    match String.sub text !pos 5 with
    | "endif" ->
        pos := !pos + 5;
        false
    | _ -> true
  else false

(**
   Parses a while loop statement from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   The function first skips any whitespace characters.
   It then checks if the current position is past the end of the string.
   If it is, it throws a ParserError.
   Otherwise, it parses the boolean expression for the while loop.

   The function then checks if the "do" keyword exists at the current position.
   If it does not, it throws a ParserError.
   If it does, it skips the "do" keyword and proceeds to parse the statements within the while loop.

   The function then checks if the "done" keyword exists at the current position.
   If it does not, it throws a ParserError.
   If it does, it skips the "done" keyword and returns the While record containing the boolean expression and the statements within the while loop.
*)
let rec parse_while_loop_statement text pos initialised_variables =
  skip_whitespaces text pos;
  if !pos > String.length text then
    throw_except
      (ParserError
         ( !count_of_newline,
           !cur_pos_on_line,
           "couldn't find bool expression in while" ))
  else
    let expression = parse_expr text pos initialised_variables in
    if check_do_exists text pos then (
      let loop =
        While
          ( expression,
            parse_statements text pos check_done_exists initialised_variables )
      in
      skip_whitespaces text pos;
      if !pos + 4 <= String.length text then
        match String.sub text !pos 4 with
        | "done" ->
            pos := !pos + 4;
            loop
        | _ ->
            throw_except
              (ParserError
                 ( !count_of_newline,
                   !cur_pos_on_line,
                   "couldn't find assign statement" ))
      else
        throw_except
          (ParserError
             (!count_of_newline, !cur_pos_on_line, "couldn't find done")))
    else
      throw_except
        (ParserError (!count_of_newline, !cur_pos_on_line, "couldn't find do"))

(**
   Parses an if statement from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

    Return: an If record containing the boolean expression, the statements within the if block, and the statements within the else block.
*)
and parse_if_statement text pos initialised_variables =
  if !pos > String.length text then
    throw_except
      (ParserError
         ( !count_of_newline,
           !cur_pos_on_line,
           "couldn't find bool expression in if" ))
  else
    let expression = parse_expr text pos initialised_variables in
    if check_then_exists text pos then (
      skip_whitespaces text pos;
      let if_fork =
        parse_statements text pos check_else_and_endif_construction_exists
          initialised_variables
      in
      skip_whitespaces text pos;
      if !pos + 5 <= String.length text then
        match String.sub text !pos 5 with
        | "endif" ->
            pos := !pos + 5;
            If (expression, if_fork, [ EmptyStatement ])
        | "else " | "else\t" | "else\n" | "else\r" ->
            pos := !pos + 5;
            If
              ( expression,
                if_fork,
                parse_statements text pos check_endif_exists_and_skip
                  initialised_variables )
        | _ ->
            throw_except
              (ParserError
                 ( !count_of_newline,
                   !cur_pos_on_line,
                   "couldn't find assign statement" ))
      else
        throw_except
          (ParserError
             (!count_of_newline, !cur_pos_on_line, "couldn't find else")))
    else
      throw_except
        (ParserError (!count_of_newline, !cur_pos_on_line, "couldn't find then"))

(**
   Parses a sequence of statements from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [check] is a function that checks if the parsing should continue.
   [initialised_variables] is a set of variables that have already been initialized.

   Return: the list of statements.
*)
and parse_statements text pos check initialised_variables =
  let all = ref [] in
  while check text pos do
    skip_whitespaces text pos;
    let ident = identifier text pos in
    match ident with
    | "while" ->
        let result =
          parse_while_loop_statement text pos initialised_variables
        in
        all := !all @ [ result ]
    | "if" ->
        let result = parse_if_statement text pos initialised_variables in
        all := !all @ [ result ]
    | "var" ->
        let result = parser_assign_statement text pos initialised_variables in
        all := !all @ [ result ]
    | "return" -> (
        let result = parse_expr_statement text pos initialised_variables in
        match result with
        | Expression ex -> all := !all @ [ ReturnStatement ex ]
        | __ ->
            throw_except
              (ParserError
                 ( !count_of_newline,
                   !cur_pos_on_line,
                   "unexpected expression " ^ string_of_statement result 0 )))
    | "break" -> all := !all @ [ BreakStatement ]
    | _ ->
        pos := !pos - String.length ident;
        let result = parse_expr_statement text pos initialised_variables in
        all := !all @ [ result ]
  done;
  !all

(**
   Checks if the current position in the string represents the end of a function's statements.

   Parameters:
   - [text]: The string representation of the program.
   - [pos]: A mutable reference to keep track of the current position in the string.

   Returns:
   - A boolean value indicating whether the current position represents the end of a function's statements.
     The function returns true if the current position is less than the length of the string and the character at the current position is not a closing curly brace '}'.
     Otherwise, it returns false.
*)

let check_func_stmts_end text pos =
  skip_whitespaces text pos;
  !pos < String.length text && not (expect_symbol text !pos '}')

(**
   Parses the statements within a function from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.
   [initialised_variables] is a set of variables that have already been initialized.

   Return: list of statements.
*)
let parse_func_stmts text pos initialised_variables =
  skip_whitespaces text pos;
  let symbol = text.[!pos] in
  match symbol with
  | '{' ->
      incr pos;
      incr cur_pos_on_line;
      let func_stmts =
        parse_statements text pos check_func_stmts_end initialised_variables
      in
      func_stmts
  | _ ->
      throw_except
        (ParserError
           ( !count_of_newline,
             !cur_pos_on_line,
             "unexpected symbol: " ^ Char.escaped symbol ^ "without '{'" ))

(**
   Parses a function structure from a given string.

   [text] is the string representation of the program.
   [pos] is a mutable reference to keep track of the current position in the string.

    Return: a FuncStruct record containing the function name, the list of function arguments, and the list of statements within the function.
*)
let parse_func_structure text pos =
  skip_whitespaces text pos;
  let ident = identifier text pos in
  if String.length ident = 0 then
    throw_except
      (ParserError
         (!count_of_newline, !cur_pos_on_line, "unnamed function define"));
  if StringSet.mem ident !initialised_functions then
    throw_except
      (LogicErrorParsing
         ( !count_of_newline,
           !cur_pos_on_line,
           "multidefinition of function: " ^ ident ));

  initialised_functions := StringSet.add ident !initialised_functions;
  let initialised_variables = ref StringSet.empty in
  skip_whitespaces text pos;
  let symbol = text.[!pos] in
  match symbol with
  | '(' -> (
      incr pos;
      incr cur_pos_on_line;
      skip_whitespaces text pos;
      let func_args_expr : string list ref = ref [] in
      while text.[!pos] != ')' do
        skip_whitespaces text pos;
        let arg = identifier text pos in
        if String.length arg = 0 then
          throw_except
            (ParserError
               ( !count_of_newline,
                 !cur_pos_on_line,
                 "unnamed argument in function define " ^ ident ));
        func_args_expr := !func_args_expr @ [ arg ];
        initialised_variables := StringSet.add arg !initialised_variables;
        skip_whitespaces text pos;
        if expect_symbol text !pos ',' || expect_symbol text !pos ')' then
          if expect_symbol text !pos ',' then (
            incr pos;
            incr cur_pos_on_line)
          else ()
        else
          throw_except
            (ParserError
               ( !count_of_newline,
                 !cur_pos_on_line,
                 "couldn't find symbol of closed bracket for close function '"
                 ^ ident ^ "' call: ')' or ','" ))
      done;
      functions_args_count :=
        StringMap.add ident (List.length !func_args_expr) !functions_args_count;
      skip_whitespaces text pos;
      match expect_symbol text !pos ')' with
      | true ->
          incr pos;
          incr cur_pos_on_line;
          let block_of_func = parse_func_stmts text pos initialised_variables in
          FuncStruct (ident, !func_args_expr, block_of_func)
      | false ->
          throw_except
            (ParserError
               ( !count_of_newline,
                 !cur_pos_on_line,
                 "couldn't find symbol of closed bracket for close function '"
                 ^ ident ^ "' call: ')'" )))
  | _ ->
      throw_except
        (ParserError
           ( !count_of_newline,
             !cur_pos_on_line,
             "couldn't find symbol of open bracket of function call:"
             ^ Char.escaped symbol ))

(**
  Parses a program represented as a string.

  [text] The string representation of the program.
  [pos] A mutable reference to keep track of the current position in the string.
  [check] A function that checks if the parsing should continue.
  Return: the result of parsing the structures in the program.
*)
let parse_structures text pos check =
  let all = ref [] in
  while check text pos do
    skip_whitespaces text pos;
    let ident = identifier text pos in
    match ident with
    | "def" ->
        let result = parse_func_structure text pos in
        if !pos < String.length text then (
          incr pos;
          incr cur_pos_on_line)
        else ();
        all := !all @ [ result ]
    | _ ->
        throw_except
          (ParserError
             ( !count_of_newline,
               !cur_pos_on_line,
               "unexpected identifer: " ^ ident ))
  done;
  !all

let check_program_end text pos =
  skip_whitespaces text pos;
  !pos < String.length text

(**
   Parse a program represented as a string.

   [text] is the string representation of the program.

   The function starts parsing from the beginning of the string. It uses a mutable reference [pos] to keep track of the current position in the string.

   The function calls [parse_structures] with the provided [text], [pos], and [check_program_end] as arguments.

   [check_program_end] is a function that checks if the parsing should continue. In this case, it checks if the current position is less than the length of the string.

   Return: the result of parsing the structures in the program.
*)
let parse_program text =
  let pos = ref 0 in
  parse_structures text pos check_program_end
