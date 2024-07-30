open Exceptions
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type oper =
  | Plus
  | Multiply
  | Divide
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
  | _ -> throw_except (ASTError "unexpected unary operator")
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

let rec string_of_statements stmts =
  let str_lst = List.map (fun stmt -> string_of_statement stmt) stmts in
  String.concat "\n" str_lst

and string_of_statement = function
  | EmptyStatement -> ""
  | ReturnStatement e -> "return " ^ string_of_expression e ^ ";"
  | Expression e -> string_of_expression e ^ ";"
  | AssignStatement (v, e2) ->
      "var " ^ v ^ " := " ^ string_of_expression e2 ^ ";"
  | While (e1, stmts) ->
      "while " ^ string_of_expression e1 ^ " do\n" ^ string_of_statements stmts
      ^ "\ndone"
  | If (e1, if_stmts, else_stmts) ->
      if else_stmts = [ EmptyStatement ] then
        "if " ^ string_of_expression e1 ^ " then\n"
        ^ string_of_statements if_stmts
        ^ "\nendif"
      else
        "if " ^ string_of_expression e1 ^ " then\n"
        ^ string_of_statements if_stmts
        ^ "\nelse\n"
        ^ string_of_statements else_stmts
        ^ "\nendif"

let initialised_functions =
  ref (StringSet.of_list [ "_start"; "print_int"; "read_char"; "read_int" ])

let functions_args_count : int StringMap.t ref =
  ref
    StringMap.(
      empty |> add "print_int" 1 |> add "read_char" 0 |> add "read_int" 0)

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' | '\r' | '\t' | '\n' -> true | _ -> false
let is_newline = function '\n' -> true | _ -> false

let expect_symbol text pos symbol =
  if pos >= 0 && pos < String.length text then text.[pos] == symbol else false

let count_of_newline = ref 0
let cur_pos_on_line = ref 0

let skip_whitespaces text pos =
  let length = String.length text in
  while !pos < length && is_whitespace text.[!pos] do
    if is_newline text.[!pos] then (
      (*  print_endline (string_of_int !count_of_newline); *)
      incr count_of_newline;
      cur_pos_on_line := 0)
    else ();
    incr pos;
    incr cur_pos_on_line
  done

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

let check_expr_stmt_close text pos expression =
  skip_whitespaces text pos;
  if check_exists_simple_stmt_close text pos then Expression expression
  else
    throw_except
      (ParserError
         ( !count_of_newline,
           !cur_pos_on_line,
           "couldn't find close symbol of expression statement: ';'" ))

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

let parse_expr_statement text pos initialised_variables =
  skip_whitespaces text pos;
  let result = parse_expr text pos initialised_variables in
  match result with
  | EmptyExpression -> EmptyStatement
  | _ -> check_expr_stmt_close text pos result

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

let check_done_exists text pos =
  skip_whitespaces text pos;
  if !pos + 4 < String.length text then
    match String.sub text !pos 4 with "done" -> false | _ -> true
  else false

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
                   "unexpected expression " ^ string_of_statement result )))
    | _ ->
        pos := !pos - String.length ident;
        let result = parse_expr_statement text pos initialised_variables in
        all := !all @ [ result ]
  done;
  !all

let check_func_stmts_end text pos =
  skip_whitespaces text pos;
  !pos < String.length text && not (expect_symbol text !pos '}')

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

let parse_program text =
  let pos = ref 0 in
  (* let e = parse_structures text pos check_program_end in
     List.iter (fun i -> print_endline (show_structure i)) e;
     e *)
  parse_structures text pos check_program_end