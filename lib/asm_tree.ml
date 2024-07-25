open Parser
(* open Sexplib *)

module StringMap = Map.Make (String)

(* [@@deriving sexp] *)
(* [@@deriving show] *)
type mp = int StringMap.t
(* [@@print
     fun pp fmt map ->
       Format.fprintf fmt "@[<v>{";
       iter (fun key value -> Format.fprintf fmt "@ %s -> %d;@ " key value) map;
       Format.fprintf fmt "@]}"]
   [@@deriving show] *)

type reg = ZERO | SP | RA | V | GP | TP | T of int | A of int | S of int
[@@deriving show]

type instr =
  | J of string
  | Li of reg * int
  | La of reg * reg
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
  | Label of string * instr list
  | Comment of string
  | InvalidInstruction
[@@deriving show]

type structure = Function of string * instr list * mp
(* [@@deriving show] *)

let variables_shifts : int StringMap.t ref = ref StringMap.empty

let init_variables (cur_stack_pointer : int ref) (statements : statement list) =
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

let map_assign op =
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

let binop_to_asm op reg1 reg2 =
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
(* Printf.sprintf "%s\nld a4, -%d(s0)\n%s\nsd a5, -%d(s0)" expr_asm
    var_pos op_asm var_pos) *)

let rec expr_to_asm e cur_stack_pointer variables_shifts =
  match e with
  | AssignExpression (v, op, ex) -> (
      print_endline "dff";
      let var_pos = StringMap.find v !variables_shifts in
      print_endline "sd";
      let expr_asm = expr_to_asm ex cur_stack_pointer variables_shifts in
      match op with
      | DefaultAssign ->
          print_endline "ss";
          expr_asm @ [ Sd (A 5, var_pos) ]
      | _ ->
          print_endline "ssd";
          let search_oper = map_assign op in
          let op_asm = binop_to_asm search_oper (A 5) (A 4) in
          expr_asm @ [] @ op_asm)
  | Number n -> [ Li (A 5, n) ]
  | Variable v ->
      let var_pos = StringMap.find v !variables_shifts in
      [ Ld (A 5, var_pos) ]
  | Unary (op, ex) -> (
      let ex_asm = expr_to_asm ex cur_stack_pointer variables_shifts in
      match op with
      | Plus -> ex_asm
      | Minus -> ex_asm @ [ Neg (A 5, A 5) ]
      | _ ->
          failwith
            ("ASTError: unexpected unary operator: "
            ^ string_of_binary_operator op
            ^ "."))
  | Binary (ex1, op, ex2) ->
      let asm1 = expr_to_asm ex1 cur_stack_pointer variables_shifts in
      cur_stack_pointer := !cur_stack_pointer + 8;
      let asm2 = expr_to_asm ex2 cur_stack_pointer variables_shifts in
      let full_asm =
        asm1
        @ [ Sd (A 5, !cur_stack_pointer); Ld (A 4, !cur_stack_pointer) ]
        @ asm2
      in
      cur_stack_pointer := !cur_stack_pointer - 4;
      full_asm @ binop_to_asm op (A 5) (A 4)
  | EmptyExpression -> [ Comment "# Empty expression" ]

let rec stmt_to_asm s cur_stack_pointer count_of_while count_of_if
    open_label_count variables_shifts =
  match s with
  | Expression ex -> (
      match ex with
      | AssignExpression (_, _, _) ->
          expr_to_asm ex cur_stack_pointer variables_shifts
      | _ ->
          failwith
            ("ASTError: unsupported  expression statement: "
            ^ string_of_expression ex ex ex
            ^ ";."))
  | AssignStatement (v, ex) ->
      let var_pos = StringMap.find v !variables_shifts in
      let asm = expr_to_asm ex cur_stack_pointer variables_shifts in
      asm @ [ Sd (A 5, var_pos) ]
  | While (ex, stmts) ->
      while_loop_to_asm ex stmts cur_stack_pointer count_of_while count_of_if
        open_label_count variables_shifts
  | If (ex, then_stmts, else_stmts) ->
      if_stmt_to_asm ex then_stmts else_stmts cur_stack_pointer count_of_while
        count_of_if open_label_count variables_shifts
  | EmptyStatement -> [ Comment "# Empty Statement" ]

and stmts_to_asm stmts cur_stack_pointer count_of_while count_of_if
    open_label_count variables_shifts =
  let stmts_asm = ref [] in
  List.iter
    (fun stmt ->
      let stmt_asm =
        stmt_to_asm stmt cur_stack_pointer count_of_while count_of_if
          open_label_count variables_shifts
      in

      stmts_asm := !stmts_asm @ stmt_asm)
    stmts;
  !stmts_asm

and while_loop_to_asm e stmts cur_stack_pointer count_of_while count_of_if
    open_label_count variables_shifts =
  incr count_of_while;
  let cur_while_index = !count_of_while in
  let while_condition_label =
    Printf.sprintf ".while_%d_condition" cur_while_index
  in
  let while_loop_label = Printf.sprintf ".while_%d_loop" cur_while_index in
  let exp_while = expr_to_asm e cur_stack_pointer variables_shifts in
  let stmts_asm =
    stmts_to_asm stmts cur_stack_pointer count_of_while count_of_if
      open_label_count variables_shifts
  in

  [
    Label (while_loop_label, stmts_asm); Label (while_condition_label, exp_while);
  ]

and if_stmt_to_asm ex then_stmts else_stmts cur_stack_pointer count_of_while
    count_of_if open_label_count variables_shifts =
  incr count_of_if;
  incr open_label_count;
  let current_open_label_index = !count_of_if in
  let current_if_index = !count_of_if in
  let ex_asm = expr_to_asm ex cur_stack_pointer variables_shifts in
  let then_stmts_asm =
    stmts_to_asm then_stmts cur_stack_pointer count_of_while count_of_if
      open_label_count variables_shifts
  in
  let else_stmts_asm =
    stmts_to_asm else_stmts cur_stack_pointer count_of_while count_of_if
      open_label_count variables_shifts
  in

  let else_branch_label_name = Printf.sprintf ".if_%d_else" current_if_index in
  let next_open_label_name = ".L" ^ string_of_int current_open_label_index in
  ex_asm
  @ [ Beq (A 5, ZERO, else_branch_label_name) ]
  @ then_stmts_asm @ [ J next_open_label_name ]
  @ [ Label (else_branch_label_name, else_stmts_asm) ]
  @ [ J next_open_label_name ]
  @ [ Label (next_open_label_name, []) ]

let cur_stack_pointer = ref 16
let st_stack_pointer = ref 16
let count_of_while = ref 0
let count_of_if = ref 0
let open_label_count = ref 0

let rec asm_stmt stmt list_of_instr cur_stack_pointer variables_shifts =
  match stmt with
  | Expression ex ->
      let result = expr_to_asm ex cur_stack_pointer variables_shifts in
      list_of_instr := !list_of_instr @ result
  | AssignStatement (_, ex) ->
      let result = expr_to_asm ex cur_stack_pointer variables_shifts in
      list_of_instr := !list_of_instr @ result
  | While (e, stmts) ->
      let result = expr_to_asm e cur_stack_pointer variables_shifts in
      list_of_instr := !list_of_instr @ result;
      let loop_stmts =
        while_loop_to_asm e stmts cur_stack_pointer count_of_while count_of_if
          open_label_count variables_shifts
      in
      list_of_instr := !list_of_instr @ loop_stmts
  | If (e, if_stmts, else_stmts) ->
      let result = expr_to_asm e cur_stack_pointer variables_shifts in
      list_of_instr := !list_of_instr @ result;
      let if_then_statement =
        if_stmt_to_asm e if_stmts else_stmts cur_stack_pointer count_of_while
          count_of_if open_label_count variables_shifts
      in
      list_of_instr := !list_of_instr @ if_then_statement
  | EmptyStatement ->
      list_of_instr := !list_of_instr @ [ Comment "# Empty Statement" ]

and asm_tree stmts cur_stack_pointer =
  let all = ref ([] : structure list) in
  (* TO DO: all move from this function *)
  let list_of_instr = ref [] in
  variables_shifts := init_variables cur_stack_pointer stmts;
  (* print_endline "=====";
     StringMap.iter
       (fun key value -> print_endline (key ^ ": " ^ string_of_int value))
       !variables_shifts;
     print_endline "====="; *)
  List.iter
    (fun stmt -> asm_stmt stmt list_of_instr cur_stack_pointer variables_shifts)
    stmts;
  let cur_structure = Function ("_start", !list_of_instr, !variables_shifts) in
  all := !all @ [ cur_structure ];
  !all

(* let show = show_structure (List.nth show_res 0) |> print_endline *)

(* let s = [%yojson_of: int M.t] m |> Yojson.Safe.to_string *)
