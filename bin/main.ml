open Compiler.Parser

(* open Compiler.Optimizator *)
open Compiler.Asm_tree
open Compiler.Riscv_translator

module Main = struct
  let append_to_file filename content =
    let oc = open_out filename in
    output_string oc content;
    close_out oc

  let text =
    {|

  def sum(a,b,c,d,e,f,g,h,i,k) {
    return a + b + c + d + e + g + f + h + i + k;
  }

  def bigsum(a,b,c,d,e,f,g,h,i,k, o,p) {
    return a + b + c + d + e + g + f + h + i + k + o + p;
  }
  
  def factorial(n) {
    var acc:=1;
    while n>1 do
        acc:=acc*n;
        n:=n-1;
    done
    return acc;
}

  def combinations(n,k) {
      return factorial(n) / (factorial(k) * factorial(n - k));
  }

  def nsum(n) {
    if n <= 0 then 
      return 0;
    endif
    
    return n + nsum(n - 1);
  }

  def main() {
    print_int(sum(1,2,3,4,5,6,7,8,9,10));
    print_int(bigsum(0,1,2,3,4,5,6,7,8,9,10, 11));
    print_int(combinations(10, 5));
    print_int(nsum(10));
    return 0;
  }
  |}

  let compile out_filename =
    let content =
      parse_program text |> program_to_asm_tree |> string_of_instr_list
    in
    append_to_file out_filename content

  let () = compile "lang/main.s"
  (* let program = parse_program text in
     (* List.iter (fun st -> show_structure st |> print_endline) program; *)
     let instructions = program_to_asm_tree program in
     (* List.iter
        (fun instruction -> print_endline (show_instr instruction))
        instructions; *)
     print_endline (string_of_instr_list instructions) *)
end
