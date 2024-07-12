module Main = struct
  let text =
    {|
  acc:=1;
   n:=6;
   while n do
    acc :=          acc * 10;
    n:=n-1;
  done
   while n do
    acc :=          acc * 10;
    n:=n-1;
  done
  |}

  let pos = ref 0
  let res = Compiler.Parser.global_statements text pos
  let () = Compiler.Parser.stmts_to_string text pos res |> print_endline
end
