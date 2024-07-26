open Parser

type reg =
  | TemporaryReg of int
  | ArgumentReg of int
  | CallSafetyReg of int
  | FramePointer
  | StackPointer
  | ReturnAddress
  | Zero
[@@deriving show]

type instr =
  | Li of reg * int
  | Ld of reg * int
  | Sd of reg * int
  | Mv of reg * reg
  | Neg of reg * reg
  | Add of reg * reg * reg
  | Mul of reg * reg * reg
  | Sub of reg * reg * reg
  | Div of reg * reg * reg
  | Sgt of reg * reg * reg
  | Slt of reg * reg * reg
  | Xori of reg * reg * int
  | Seqz of reg * reg
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Label of string
  | Jump of string
  | Call of string
[@@deriving show]

type structure = Function of string * instr list * int StringMap.t

let init_variables (cur_stack_pointer : int ref) (statements : statement list) :
    int StringMap.t =
  let variables_stack_position : int StringMap.t ref = ref StringMap.empty in
  List.iter
    (fun stmt ->
      match stmt with
      | AssignStatement (v, _) ->
          cur_stack_pointer := !cur_stack_pointer + 8;
          variables_stack_position :=
            StringMap.add v !cur_stack_pointer !variables_stack_position
      | _ -> ())
    statements;
  !variables_stack_position

let map_assign (op : assign_oper) : oper =
  match op with
  | DefaultAssign -> Invalid
  | PlusAssign -> Plus
  | MinusAssign -> Minus
  | MultiplyAssign -> Multiply
  | DivideAssign -> Divide
  | InvalidAssing ->
      failwith
        ("ASTError: unexpected assign operator: "
        ^ string_of_assign_operator op
        ^ ".")

let binop_to_asm (op : oper) (reg1 : reg) (reg2 : reg) : instr list =
  match op with
  | Plus -> [ Add (reg1, reg2, reg1) ]
  | Minus -> [ Sub (reg1, reg2, reg1) ]
  | Multiply -> [ Mul (reg1, reg2, reg1) ]
  | Divide -> [ Div (reg1, reg2, reg1) ]
  | More -> [ Sgt (reg1, reg2, reg1) ]
  | Low -> [ Slt (reg1, reg2, reg1) ]
  | MoreOrEqual -> [ Slt (reg1, reg2, reg1); Xori (reg1, reg2, 1) ]
  | LowOrEqual -> [ Sgt (reg1, reg2, reg1); Xori (reg1, reg2, 1) ]
  | Equal -> [ Sub (reg1, reg2, reg1); Seqz (reg1, reg2) ]
  | Unequal ->
      [ Sub (reg1, reg2, reg1); Seqz (reg1, reg2); Xori (reg1, reg2, 1) ]
  | Invalid ->
      failwith
        ("ASTError: unexpected binary operator: "
        ^ string_of_binary_operator op
        ^ ".")

let rec expr_to_asm_tree (ex : expr) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) : instr list =
  match ex with
  | Number n -> [ Li (ArgumentReg 0, n) ]
  | Variable v ->
      let var_stack_position = StringMap.find v !variables_stack_position in
      [ Ld (ArgumentReg 0, var_stack_position) ]
  | Unary (op, subex) -> (
      let subex_asm_tree =
        expr_to_asm_tree subex stack_pointer variables_stack_position
      in
      match op with
      | Plus -> subex_asm_tree
      | Minus -> subex_asm_tree @ [ Neg (ArgumentReg 0, ArgumentReg 0) ]
      | _ ->
          failwith
            ("ASTError: unexpected unary operator: "
            ^ string_of_binary_operator op
            ^ "."))
  | Binary (subex1, op, subex2) ->
      let subex1_asm_tree =
        expr_to_asm_tree subex1 stack_pointer variables_stack_position
      in
      stack_pointer := !stack_pointer + 8;
      (*TODO: MagicNumber: 8. It is word byte-size in 64bits RISC-V) *)
      let subex2_asm_tree =
        expr_to_asm_tree subex2 stack_pointer variables_stack_position
      in
      let ex_eval_asm_tree =
        subex1_asm_tree
        @ [ Sd (ArgumentReg 0, !stack_pointer) ]
        @ subex2_asm_tree
        @ [ Ld (ArgumentReg 1, !stack_pointer) ]
      in
      stack_pointer := !stack_pointer - 8;
      (*TODO: MagicNumber: 8. It is word byte-size in 64bits RISC-V) *)
      ex_eval_asm_tree @ binop_to_asm op (ArgumentReg 0) (ArgumentReg 1)
  | AssignExpression (v, op, subex) -> (
      let var_stack_position = StringMap.find v !variables_stack_position in
      let subex_asm_tree =
        expr_to_asm_tree subex stack_pointer variables_stack_position
      in
      let save_result_asm_tree = [ Sd (ArgumentReg 0, var_stack_position) ] in
      match op with
      | DefaultAssign -> subex_asm_tree @ save_result_asm_tree
      | InvalidAssing ->
          failwith
            ("ASTError: unexpected assign operator: "
            ^ string_of_assign_operator op
            ^ ".")
      | _ ->
          let op_asm_tree =
            binop_to_asm (map_assign op) (ArgumentReg 0) (ArgumentReg 1)
          in
          subex_asm_tree
          @ [ Ld (ArgumentReg 1, var_stack_position) ]
          @ op_asm_tree @ save_result_asm_tree)
  | FuncCall (name, expressions) -> 

    let all_instr = ref [] in 
    if List.length expressions = 1 then (expr_to_asm_tree (List.nth expressions 0) stack_pointer variables_stack_position @ [Call name]) else ( 
    List.iter(fun exp -> 
      all_instr := !all_instr @ (expr_to_asm_tree exp stack_pointer variables_stack_position );
      stack_pointer := !stack_pointer + 8;
      all_instr := !all_instr @ [Sd (ArgumentReg 0, !stack_pointer)]
      )
      (List.rev expressions);
    let length = ref( List.length expressions) in
    let regInd = ref 0 in
    while !length > 0 && !regInd < 8 do (
    decr length; 
    all_instr := !all_instr @ [Ld(ArgumentReg !regInd, !stack_pointer)];
    incr regInd;
    stack_pointer := !stack_pointer - 8;
    
    ) done;
    stack_pointer := !stack_pointer - 8 * !length;
    !all_instr @ [Call name])
  | EmptyExpression -> []

let rec statement_to_asm_tree (stmt : statement) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) (label_count : int ref) :
    instr list =
  match stmt with
  | Expression ex -> (
      match ex with
        | AssignExpression (_, _, _) ->
          expr_to_asm_tree ex stack_pointer variables_stack_position
        | FuncCall (_, _)  -> 
          expr_to_asm_tree ex stack_pointer variables_stack_position
        | _ ->
          failwith
            ("ASTError: unsupported  expression statement: "
            ^ string_of_expression ex
            ^ ";."))
  | AssignStatement (v, ex) ->
      let var_stack_position = StringMap.find v !variables_stack_position in
      let subex_asm_tree =
        expr_to_asm_tree ex stack_pointer variables_stack_position
      in
      subex_asm_tree @ [ Sd (ArgumentReg 0, var_stack_position) ]
  | While (ex, stmts) ->
      while_loop_to_asm ex stmts stack_pointer variables_stack_position
        label_count
  | If (ex, then_stmts, else_stmts) ->
      if_stmt_to_asm ex then_stmts else_stmts stack_pointer
        variables_stack_position label_count
  | _ -> []

and while_loop_to_asm (ex : expr) (stmts : statement list)
    (stack_pointer : int ref) (variables_stack_position : int StringMap.t ref)
    (label_count : int ref) : instr list =
  incr label_count;
  let cur_while_index = !label_count in
  let while_condition_label_name =
    Printf.sprintf ".while_%d_condition" cur_while_index
  in
  let while_loop_label_name = Printf.sprintf ".while_%d_loop" cur_while_index in
  let while_condition_expr_tree =
    expr_to_asm_tree ex stack_pointer variables_stack_position
  in
  let while_loop_stmts_asm_tree =
    stmts_to_asm_tree stmts stack_pointer variables_stack_position label_count
  in
  [ Jump while_condition_label_name; Label while_loop_label_name ]
  @ while_loop_stmts_asm_tree
  @ [ Jump while_condition_label_name; Label while_condition_label_name ]
  @ while_condition_expr_tree
  @ [ Bne (ArgumentReg 0, Zero, while_loop_label_name) ]

and if_stmt_to_asm (ex : expr) (then_stmts : statement list)
    (else_stmts : statement list) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) (label_count : int ref) :
    instr list =
  let current_if_index = !label_count + 1 in
  let result_label_index = !label_count + 2 in
  label_count := !label_count + 2;
  let ex_asm_tree =
    expr_to_asm_tree ex stack_pointer variables_stack_position
  in
  let then_stmts_asm_tree =
    stmts_to_asm_tree then_stmts stack_pointer variables_stack_position
      label_count
  in
  let else_stmts_asm_tree =
    stmts_to_asm_tree else_stmts stack_pointer variables_stack_position
      label_count
  in
  let else_branch_label_name = Printf.sprintf ".if_%d_else" current_if_index in
  let next_open_label_name = ".L" ^ string_of_int result_label_index in
  ex_asm_tree
  @ [ Beq (ArgumentReg 0, Zero, else_branch_label_name) ]
  @ then_stmts_asm_tree
  @ [ Jump next_open_label_name; Label else_branch_label_name ]
  @ else_stmts_asm_tree
  @ [ Jump next_open_label_name; Label next_open_label_name ]

and stmts_to_asm_tree (stmts : statement list) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) (label_count : int ref) :
    instr list =
  let stmts_asm = ref [] in
  List.iter
    (fun stmt ->
      let stmt_asm_tree =
        statement_to_asm_tree stmt stack_pointer variables_stack_position
          label_count
      in
      stmts_asm := !stmts_asm @ stmt_asm_tree)
    stmts;
  !stmts_asm

let program_to_asm_tree (stmts : statement list) : instr list =
  let stack_pointer = ref 16 in
  let variables_stack_position = ref (init_variables stack_pointer stmts) in
  let label_count = ref 0 in
  (* TODO: label_count - its not fun info its program info *)
  let stmts_asm_tree =
    stmts_to_asm_tree stmts stack_pointer variables_stack_position label_count
  in
  (* must return: Function ("_start", stmts_asm_tree, variables_stack_position) *)
  stmts_asm_tree
