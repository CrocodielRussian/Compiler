type except = ParserError of int * int * string | LogicErrorParsing of int * int * string | ASTError of string | LogicErrorMath of string 

let string_of_exception = function
| ParserError(line, position, message) -> Printf.sprintf "ParseError: on line %d on position %d: %s." line position message
| LogicErrorParsing(line, position, message) -> Printf.sprintf "LogicError: on line %d on position %d: %s." line position message
| ASTError(message) -> Printf.sprintf "ASTError: unexpected AST element %s." message
| LogicErrorMath(message) -> Printf.sprintf "LogicErrorMath: %s" message
(* | _ -> "TO DO" *)

let throw_except (ex : except)=
  failwith (string_of_exception ex)
