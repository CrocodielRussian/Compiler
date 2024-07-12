val is_alpha : char -> bool
val is_digit : char -> bool
val is_whitespace : char -> bool
val skip_whitespaces : string -> int ref -> unit
val positive_number : string -> int ref -> Parser.ExpressionParser.expr
val op_add : char -> oper
val op_mult : char -> oper
val parse_expr_mul : string -> int ref -> expr
val parse_expr : string -> int ref -> expr
val simplest_expr : string -> int ref -> expr
val expr_to_string : 'a -> int ref -> expr -> string
