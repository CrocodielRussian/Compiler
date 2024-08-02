type except =
  | ParserError of int * int * string
  | LogicErrorParsing of int * int * string
  | ASTError of string
  | LogicErrorMath of string

(**
    Convert an exception to a human-readable string.
*)
let string_of_exception = function
  | ParserError (line, position, message) ->
      Printf.sprintf "ParseError: on line %d on position %d: %s." line position
        message
  | LogicErrorParsing (line, position, message) ->
      Printf.sprintf "LogicError: on line %d on position %d: %s." line position
        message
  | ASTError message ->
      Printf.sprintf "ASTError: unexpected AST element %s." message
  | LogicErrorMath message -> Printf.sprintf "LogicErrorMath: %s" message

(**
    Throw an exception with the given type and message.
    
    @param ex The exception to be thrown.

    @raise Failure with exeption
*)
let throw_except (ex : except) = failwith (string_of_exception ex)
