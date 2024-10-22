open Parser
open Exceptions

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
  | FrameLd of reg * int
  | FrameSd of reg * int
  | StackPointerLd of reg * int
  | StackPointerSd of reg * int
  | Mv of reg * reg
  | Neg of reg * reg
  | Not of reg * reg
  | Add of reg * reg * reg
  | Mul of reg * reg * reg
  | Sub of reg * reg * reg
  | Div of reg * reg * reg
  | Rem of reg * reg * reg
  | Sgt of reg * reg * reg
  | Slt of reg * reg * reg
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Xori of reg * reg * int
  | Addi of reg * reg * int
  | Seqz of reg * reg
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Label of string
  | GlobalModifier of string
  | Jump of string
  | Call of string
  | Ret
  | Nop
  | EnvCall
[@@deriving show]

(**
  [init_variables] is a recursive function that initializes the stack pointer and 
  maintains a map of variable names to their corresponding stack positions.

  @param [variables_stack_position]: A reference to a mutable map that stores variable names as keys and their corresponding stack positions as values.
  @param [stack_pointer]: A reference to an integer representing the current stack pointer position.
  @param [statements]: A list of statements to be processed.
**)
let rec init_variables (variables_stack_position : int StringMap.t ref)
    (stack_pointer : int ref) (statements : statement list) =
  List.iter
    (fun stmt ->
      match stmt with
      | AssignStatement (v, _) ->
          stack_pointer := !stack_pointer + 8;
          variables_stack_position :=
            StringMap.add v !stack_pointer !variables_stack_position
      | While (_, stmts) ->
          init_variables variables_stack_position stack_pointer stmts
      | If (_, stmts1, stmts2) ->
          init_variables variables_stack_position stack_pointer stmts1;
          init_variables variables_stack_position stack_pointer stmts2
      | _ -> ())
    statements

(**
  [max_min_variable_position] is a function that calculates the maximum and minimum positions 
  of variables in the given [variables_stack_position] map. It iterates over the map and updates 
  the [max_pos] and [min_pos] variables accordingly. If the [max_pos] is less than the [min_pos], 
  it returns the maximum and maximum positions. Otherwise, it returns the minimum and maximum positions.

  @param [variables_stack_position]: A map containing variable names as keys and their corresponding stack positions as values.

  @return: A tuple containing the maximum and minimum positions of variables in the map.
**)
let max_min_variable_position (variables_stack_position : int StringMap.t) =
  let max_pos = ref 16 in
  let min_pos = ref 24 in
  StringMap.iter
    (fun _ value ->
      max_pos := max !max_pos value;
      min_pos := min !min_pos value)
    variables_stack_position;
  if !max_pos < !min_pos then (!max_pos, !max_pos) else (!min_pos, !max_pos)
(**
   [map_assign] is a function that maps an [assign_oper] to an [oper]. It is used to handle different types of assignment operations.

   @param [op]: An [assign_oper] representing the type of assignment operation.

   @return An [oper] representing the corresponding operation. 
   @raise ASTError If the input [op] is [InvalidAssing].
**)
let map_assign (op : assign_oper) : oper =
  match op with
  | DefaultAssign -> Invalid
  | PlusAssign -> Plus
  | MinusAssign -> Minus
  | MultiplyAssign -> Multiply
  | DivideAssign -> Divide
  | InvalidAssing ->
      throw_except
        (ASTError ("unexpected assign operator: " ^ string_of_assign_operator op))

(**
  Converts a binary operation into a list of assembly instructions.

  @param op The binary operation to convert.
  @param reg1 The first register to use in the assembly instructions.
  @param reg2 The second register to use in the assembly instructions.

  @return A list of assembly instructions representing the binary operation.

  @raise ASTError If an unexpected binary operator is encountered.
*)
let binop_to_asm (op : oper) (reg1 : reg) (reg2 : reg) : instr list =
  match op with
  | Plus -> [ Add (reg1, reg2, reg1) ]
  | Minus -> [ Sub (reg1, reg2, reg1) ]
  | Multiply -> [ Mul (reg1, reg2, reg1) ]
  | Divide -> [ Div (reg1, reg2, reg1) ]
  | Mod -> [ Rem (reg1, reg2, reg1) ]
  | More -> [ Sgt (reg1, reg2, reg1) ]
  | Low -> [ Slt (reg1, reg2, reg1) ]
  | MoreOrEqual -> [ Slt (reg1, reg2, reg1); Xori (reg1, reg1, 1) ]
  | LowOrEqual -> [ Sgt (reg1, reg2, reg1); Xori (reg1, reg1, 1) ]
  | Equal -> [ Sub (reg1, reg2, reg1); Seqz (reg1, reg1) ]
  | Unequal ->
      [ Sub (reg1, reg2, reg1); Seqz (reg1, reg1); Xori (reg1, reg1, 1) ]
  | AndOper -> [ And (reg1, reg2, reg1) ]
  | OrOper -> [ Or (reg1, reg2, reg1) ]
  | _ -> throw_except (ASTError "unexpected binary operator")

let func_call_asm_tree (func_name : string) (args_instructions : instr list)
    (buffer_size : int) : instr list =
  args_instructions
  @
  if buffer_size > 0 then
    [
      Addi (StackPointer, StackPointer, ~-buffer_size);
      Call func_name;
      Addi (StackPointer, StackPointer, buffer_size);
    ]
  else [ Call func_name ]

(**
   [func_call_asm_tree] generates assembly instructions for a function call.
   It takes the function name, a list of assembly instructions for the arguments,
   and the size of the buffer needed for the function call.

   @param func_name [string] The name of the function to call.
   @param args_instructions [instr list] Assembly instructions for the arguments.
   @param buffer_size [int] The size of the buffer needed for the function call.

   @return [instr list] Assembly instructions for the function call.
*)
let rec expr_to_asm_tree (ex : expr) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) : instr list =
  match ex with
  | Number n -> [ Li (ArgumentReg 0, n) ]
  | Variable v ->
      let var_stack_position = StringMap.find v !variables_stack_position in
      [ FrameLd (ArgumentReg 0, var_stack_position) ]
  | Unary (op, subex) -> (
      let subex_asm_tree =
        expr_to_asm_tree subex stack_pointer variables_stack_position
      in
      match op with
      | Plus -> subex_asm_tree
      | Minus -> subex_asm_tree @ [ Neg (ArgumentReg 0, ArgumentReg 0) ]
      | NotOper -> subex_asm_tree @ [ Not (ArgumentReg 0, ArgumentReg 0) ]
      | _ ->
          throw_except
            (ASTError
               ("unexpected unary operator: " ^ string_of_binary_operator op)))
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
        @ [ FrameSd (ArgumentReg 0, !stack_pointer) ]
        @ subex2_asm_tree
        @ [ FrameLd (ArgumentReg 1, !stack_pointer) ]
      in
      stack_pointer := !stack_pointer - 8;
      (*TODO: MagicNumber: 8. It is word byte-size in 64bits RISC-V) *)
      ex_eval_asm_tree @ binop_to_asm op (ArgumentReg 0) (ArgumentReg 1)
  | AssignExpression (v, op, subex) -> (
      let var_stack_position = StringMap.find v !variables_stack_position in
      let subex_asm_tree =
        expr_to_asm_tree subex stack_pointer variables_stack_position
      in
      let save_result_asm_tree =
        [ FrameSd (ArgumentReg 0, var_stack_position) ]
      in
      match op with
      | DefaultAssign -> subex_asm_tree @ save_result_asm_tree
      | InvalidAssing ->
          throw_except
            (ASTError
               ("unexpected assign operator: " ^ string_of_assign_operator op))
      | _ ->
          let op_asm_tree =
            binop_to_asm (map_assign op) (ArgumentReg 0) (ArgumentReg 1)
          in
          subex_asm_tree
          @ [ FrameLd (ArgumentReg 1, var_stack_position) ]
          @ op_asm_tree @ save_result_asm_tree)
  | FuncCall (name, expressions) ->
      let all_instr = ref [] in
      if List.length expressions = 1 then
        let expr_asm_tree =
          expr_to_asm_tree (List.nth expressions 0) stack_pointer
            variables_stack_position
        in
        let _, max_pos = max_min_variable_position !variables_stack_position in
        let buffer_size = !stack_pointer - max_pos in
        func_call_asm_tree name expr_asm_tree buffer_size
      else (
        List.iter
          (fun exp ->
            all_instr :=
              !all_instr
              @ expr_to_asm_tree exp stack_pointer variables_stack_position;
            stack_pointer := !stack_pointer + 8;
            all_instr :=
              !all_instr @ [ FrameSd (ArgumentReg 0, !stack_pointer) ])
          (List.rev expressions);
        let length = ref (List.length expressions) in
        let regInd = ref 0 in
        while !length > 0 && !regInd < 8 do
          decr length;
          all_instr :=
            !all_instr @ [ FrameLd (ArgumentReg !regInd, !stack_pointer) ];
          incr regInd;
          stack_pointer := !stack_pointer - 8
        done;
        let _, max_pos = max_min_variable_position !variables_stack_position in
        let args_buffer_size = 8 * !length in
        let buffer_size = !stack_pointer - max_pos in
        stack_pointer := !stack_pointer - args_buffer_size;
        func_call_asm_tree name !all_instr buffer_size)
  | EmptyExpression -> []

(**
    Converts a statement into a list of assembly instructions.

    @param stmt The statement to convert.
    @param stack_pointer A reference to the current stack pointer.
    @param variables_stack_position A reference to a map containing the stack positions of variables.
    @param label_count A reference to the current label count.
    @param while_end_count A reference to the current while loop end count.
  
    @return A list of assembly instructions representing the given statement.
 *)
let rec statement_to_asm_tree (stmt : statement) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) (label_count : int ref)
    (while_end_count : int ref) : instr list =
  match stmt with
  | Expression ex -> (
      match ex with
      | AssignExpression (_, _, _) ->
          expr_to_asm_tree ex stack_pointer variables_stack_position
      | FuncCall (_, _) ->
          expr_to_asm_tree ex stack_pointer variables_stack_position
      | _ ->
          throw_except
            (ASTError
               ("unsupported  expression statement: " ^ string_of_expression ex
              ^ ";")))
  | AssignStatement (v, ex) ->
      let var_stack_position = StringMap.find v !variables_stack_position in
      let subex_asm_tree =
        expr_to_asm_tree ex stack_pointer variables_stack_position
      in
      subex_asm_tree @ [ FrameSd (ArgumentReg 0, var_stack_position) ]
  | While (ex, stmts) ->
      while_loop_to_asm ex stmts stack_pointer variables_stack_position
        label_count while_end_count
  | If (ex, then_stmts, else_stmts) ->
      if_stmt_to_asm ex then_stmts else_stmts stack_pointer
        variables_stack_position label_count while_end_count
  | ReturnStatement ex ->
      let _, max_pos = max_min_variable_position !variables_stack_position in
      let ex_asm_tree =
        expr_to_asm_tree ex stack_pointer variables_stack_position
      in
      ex_asm_tree
      @ [
          StackPointerLd (ReturnAddress, ~-(max_pos - 8));
          StackPointerLd (FramePointer, ~-(max_pos - 16));
          Addi (StackPointer, StackPointer, max_pos);
          Ret;
        ]
  | BreakStatement ->
      let end_while_label = Printf.sprintf ".while_%d_end" !while_end_count in
      [ Jump end_while_label ]
  | _ -> []
(**
    Converts a while loop into a list of assembly instructions.
    
    @param ex The expression representing the while loop condition.
    @param stmts The statements inside the while loop.
    @param stack_pointer A reference to the current stack pointer.
    @param variables_stack_position A reference to a map containing the stack positions of variables.
    @param label_count A reference to the current label count.
    @param while_end_count A reference to the current while loop end count.

    @return A list of assembly instructions representing the while loop.
 *)
and while_loop_to_asm (ex : expr) (stmts : statement list)
    (stack_pointer : int ref) (variables_stack_position : int StringMap.t ref)
    (label_count : int ref) (while_end_count : int ref) : instr list =
  incr while_end_count;
  let cur_while_index = !while_end_count in
  let result_label_index = !while_end_count in
  (* while_end_count := !while_end_count + 1; *)
  let while_condition_label_name =
    Printf.sprintf ".while_%d_condition" cur_while_index
  in
  let while_loop_label_name = Printf.sprintf ".while_%d_loop" cur_while_index in
  let while_condition_expr_tree =
    expr_to_asm_tree ex stack_pointer variables_stack_position
  in
  let while_loop_stmts_asm_tree =
    stmts_to_asm_tree stmts stack_pointer variables_stack_position label_count
      while_end_count
  in
  let next_open_label_name =
    Printf.sprintf ".while_%d_end" result_label_index
  in
  [ Jump while_condition_label_name; Label while_loop_label_name ]
  @ while_loop_stmts_asm_tree
  @ [ Jump while_condition_label_name; Label while_condition_label_name ]
  @ while_condition_expr_tree
  @ [ Bne (ArgumentReg 0, Zero, while_loop_label_name) ]
  @ [ Jump next_open_label_name; Label next_open_label_name ]

(**
    Converts a statement into a list of assembly instructions for an if statement.

    @param ex: The expression representing the condition of the if statement.
    @param then_stmts: The statements to be executed if the condition is true.
    @param else_stmts: The statements to be executed if the condition is false.
    @param stack_pointer: A reference to the current stack pointer.
    @param variables_stack_position: A reference to a map containing the stack positions of variables.
    @param label_count: A reference to the current label count.
    @param while_end_count: A reference to the current while loop end count.

    @return: A list of assembly instructions representing the if statement.
 *)
and if_stmt_to_asm (ex : expr) (then_stmts : statement list)
    (else_stmts : statement list) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) (label_count : int ref)
    (while_end_count : int ref) : instr list =
  let current_if_index = !label_count + 1 in
  let result_label_index = !label_count + 2 in
  label_count := !label_count + 2;
  let ex_asm_tree =
    expr_to_asm_tree ex stack_pointer variables_stack_position
  in
  let then_stmts_asm_tree =
    stmts_to_asm_tree then_stmts stack_pointer variables_stack_position
      label_count while_end_count
  in
  let else_stmts_asm_tree =
    stmts_to_asm_tree else_stmts stack_pointer variables_stack_position
      label_count while_end_count
  in
  let else_branch_label_name = Printf.sprintf ".if_%d_else" current_if_index in
  let next_open_label_name = ".L" ^ string_of_int result_label_index in
  ex_asm_tree
  @ [ Beq (ArgumentReg 0, Zero, else_branch_label_name) ]
  @ then_stmts_asm_tree
  @ [ Jump next_open_label_name; Label else_branch_label_name ]
  @ else_stmts_asm_tree
  @ [ Jump next_open_label_name; Label next_open_label_name ]
(**
    Converts a list of statements into a list of assembly instructions.

    @param stmts: The list of statements to convert.
    @param stack_pointer: A reference to the current stack pointer.
    @param variables_stack_position: A reference to a map containing the stack positions of variables.
    @param label_count: A reference to the current label count.
    @param while_end_count: A reference to the current while loop end count.

    @return: A list of assembly instructions representing the given statements.
 *)
and stmts_to_asm_tree (stmts : statement list) (stack_pointer : int ref)
    (variables_stack_position : int StringMap.t ref) (label_count : int ref)
    (while_end_count : int ref) : instr list =
  let stmts_asm = ref [] in
  List.iter
    (fun stmt ->
      let stmt_asm_tree =
        statement_to_asm_tree stmt stack_pointer variables_stack_position
          label_count while_end_count
      in
      stmts_asm := !stmts_asm @ stmt_asm_tree)
    stmts;
  !stmts_asm

let func_stmts_to_asm_tree stmts stack_pointer variables_stack_position
    label_count (while_end_count : int ref) =
  let stmts_asm_tree =
    stmts_to_asm_tree stmts stack_pointer variables_stack_position label_count
      while_end_count
  in
  stmts_asm_tree

(**
    Converts a list of statements into a list of assembly instructions.

    @param stmts: The list of statements to convert.
    @param stack_pointer: A reference to the current stack pointer.
    @param variables_stack_position: A reference to a map containing the stack positions of variables.
    @param label_count: A reference to the current label count.
    @param while_end_count: A reference to the current while loop end count.

    @return: A list of assembly instructions representing the given statements.
 *)
let func_to_asm_tree name args_name stmts label_count
    (while_end_count : int ref) =
  let stack_pointer = ref 16 in
  let variables_stack_position : int StringMap.t ref = ref StringMap.empty in
  let index = ref 0 in
  let all_instr = ref [] in
  while !index < List.length args_name do
    let arg_name = List.nth args_name !index in
    stack_pointer := !stack_pointer + 8;
    (if !index < 8 then (
       all_instr :=
         !all_instr @ [ FrameSd (ArgumentReg !index, !stack_pointer) ];
       variables_stack_position :=
         StringMap.add arg_name !stack_pointer !variables_stack_position)
     else
       let var_pos = -(!index - 8) * 8 in
       variables_stack_position :=
         StringMap.add arg_name var_pos !variables_stack_position);
    incr index
  done;
  init_variables variables_stack_position stack_pointer stmts;
  let func_block =
    func_stmts_to_asm_tree stmts stack_pointer variables_stack_position
      label_count while_end_count
  in
  let _, max_pos = max_min_variable_position !variables_stack_position in
  [
    GlobalModifier name;
    Label name;
    Addi (StackPointer, StackPointer, ~-max_pos);
    StackPointerSd (ReturnAddress, ~-(max_pos - 8));
    StackPointerSd (FramePointer, ~-(max_pos - 16));
    Addi (FramePointer, StackPointer, max_pos);
  ]
  @ !all_instr @ func_block

(**
    Converts a list of statements into a list of assembly instructions.

    @param stmts: The list of statements to convert.
    @param stack_pointer: A reference to the current stack pointer.
    @param variables_stack_position: A reference to a map containing the stack positions of variables.
    @param label_count: A reference to the current label count.
    @param while_end_count: A reference to the current while loop end count.

    @return: A list of assembly instructions representing the given statements.
 *)
let append_start_label (instructions : instr list ref) : instr list =
  instructions :=
    !instructions
    @ [
        GlobalModifier "_start";
        Label "_start";
        Addi (StackPointer, StackPointer, -16);
        StackPointerSd (FramePointer, -8);
        Addi (FramePointer, StackPointer, 16);
        Call "main";
        Addi (StackPointer, StackPointer, 16);
        Li (ArgumentReg 7, 93);
        EnvCall;
      ];
  !instructions
(**
    Converts a list of structure items (which represent functions) into a list of assembly instructions.

    @param structures: A list of structure items, where each item represents a function.
    @return: A list of assembly instructions representing the given functions.
*)
let program_to_asm_tree (structures : structure list) : instr list =
  let all = ref [] in
  let label_count = ref 0 in
  let while_end_count = ref 0 in
  List.iter
    (fun struct_item ->
      match struct_item with
      | FuncStruct (name, args_name, stmts) ->
          let insts =
            func_to_asm_tree name args_name stmts label_count while_end_count
          in
          all := !all @ insts)
    structures;
  append_start_label all
