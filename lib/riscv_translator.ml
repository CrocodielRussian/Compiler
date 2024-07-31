open Asm_tree

let string_of_reg (register : reg) =
  match register with
  | TemporaryReg n -> Printf.sprintf "t%d" n
  | ArgumentReg n -> Printf.sprintf "a%d" n
  | CallSafetyReg n -> Printf.sprintf "s%d" n
  | FramePointer -> "fp"
  | StackPointer -> "sp"
  | ReturnAddress -> "ra"
  | Zero -> "zero"

let string_of_instr (instruction : instr) =
  match instruction with
  | Li (r, n) -> Printf.sprintf "li\t\t%s, %d" (string_of_reg r) n
  | FrameLd (r, n) -> Printf.sprintf "ld\t\t%s, %d(fp)" (string_of_reg r) ~-n
  | FrameSd (r, n) -> Printf.sprintf "sd\t\t%s, %d(fp)" (string_of_reg r) ~-n
  | StackPointerLd (r, n) ->
      Printf.sprintf "ld\t\t%s, %d(sp)" (string_of_reg r) ~-n
  | StackPointerSd (r, n) ->
      Printf.sprintf "sd\t\t%s, %d(sp)" (string_of_reg r) ~-n
  | Mv (r1, r2) ->
      Printf.sprintf "mv\t%s, %s" (string_of_reg r1) (string_of_reg r2)
  | Neg (r1, r2) ->
      Printf.sprintf "neg\t%s, %s" (string_of_reg r1) (string_of_reg r2)
  | Not (r1, r2) ->
      Printf.sprintf "not\t%s, %s" (string_of_reg r1) (string_of_reg r2)
  | Add (r1, r2, r3) ->
      Printf.sprintf "add\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Mul (r1, r2, r3) ->
      Printf.sprintf "mul\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Sub (r1, r2, r3) ->
      Printf.sprintf "sub\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Div (r1, r2, r3) ->
      Printf.sprintf "div\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Sgt (r1, r2, r3) ->
      Printf.sprintf "sgt\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Slt (r1, r2, r3) ->
      Printf.sprintf "slt\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | And (r1, r2, r3) ->
      Printf.sprintf "and\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Or (r1, r2, r3) ->
      Printf.sprintf "or\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2)
        (string_of_reg r3)
  | Xori (r1, r2, n) ->
      Printf.sprintf "xori\t%s, %s, %d" (string_of_reg r1) (string_of_reg r2) n
  | Addi (r1, r2, n) ->
      Printf.sprintf "addi\t%s, %s, %d" (string_of_reg r1) (string_of_reg r2) n
  | Seqz (r1, r2) ->
      Printf.sprintf "seqz\t%s, %s" (string_of_reg r1) (string_of_reg r2)
  | Beq (r1, r2, l) ->
      Printf.sprintf "beq\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2) l
  | Bne (r1, r2, l) ->
      Printf.sprintf "bne\t%s, %s, %s" (string_of_reg r1) (string_of_reg r2) l
  | Label l -> Printf.sprintf "%s:" l
  | Jump l -> Printf.sprintf "j\t%s" l
  | Call l -> Printf.sprintf "call\t%s" l
  | Ret -> "ret "
  | Nop -> "nop"
  | EnvCall -> "ecall"
  | GlobalModifier name -> Printf.sprintf "\n.global %s" name

let string_of_instr_list (instr_list : instr list) =
  let all = ref [] in
  List.iter
    (fun instruction ->
      match instruction with
      | Label _ -> all := !all @ [ string_of_instr instruction ]
      | GlobalModifier _ -> all := !all @ [ string_of_instr instruction ]
      | _ -> all := !all @ [ "\t" ^ string_of_instr instruction ])
    instr_list;
  Printf.sprintf "%s\n" (String.concat "\n" !all |> String.trim)
