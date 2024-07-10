let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' -> true | _ -> false

let int_option_to_string (i : int option) : string =
  match i with Some x -> string_of_int x | None -> "None"

let text = "                   10"
let pos = ref 0
let length = String.length text

let ws () =
  while !pos<length && is_whitespace text.[!pos] do
    incr pos
  done;;

let number =
  ws();
  let acc = ref "" in
  while !pos < length && is_digit text.[!pos] do
    acc := !acc ^ String.make 1 text.[!pos];
    incr pos
  done;
  if String.length !acc > 0 then Some (int_of_string !acc) else None;;

number |> int_option_to_string  |> print_endline;;

pos := 0;;

number |> int_option_to_string  |> print_endline;;