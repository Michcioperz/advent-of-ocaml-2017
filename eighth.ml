let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let rev_input_lines chan =
  let rec loop acc =
    try
      loop (input_line chan :: acc)
    with End_of_file -> acc
  in loop []

module Strings =
  struct
    type t = string
    let compare x y =
      Pervasives.compare x y
  end

module RegisterMap = struct
  include Map.Make(Strings)
  let find_or_zero x m =
    match find_opt x m with
    | Some y -> y
    | None -> 0
  let execute m ((target_reg, operation), (condition_reg, condition)) =
    if condition (find_or_zero condition_reg m) then
      add target_reg (operation (find_or_zero target_reg m)) m
    else
      m
  let fold_exe m ls =
    List.fold_left execute m ls
  let max_value m =
    fold (fun _ -> max) m 0
end

type instruction = (string * (int -> int)) * (string * (int -> bool))

let parse_instruction str =
  let parts = String.split_on_char ' ' str in
  let target_reg :: parts = parts in
  let op :: parts = parts in
  let op = match op with
  | "inc" -> ( + )
  | "dec" -> ( - )
  | x -> failwith x
  in
  let operand :: parts = parts in
  let operand = int_of_string operand in
  let operation = fun x -> op x operand in
  let word_if :: parts = parts in
  let () = assert (word_if = "if") in
  let condition_reg :: parts = parts in
  let condition_op :: parts = parts in
  let condition_op = match condition_op with
  | ">" -> ( > )
  | "<" -> ( < )
  | ">=" -> ( >= )
  | "<=" -> ( <= )
  | "==" -> ( = )
  | "!=" -> ( <> )
  | x -> failwith x
  in
  let condition_operand :: parts = parts in
  let condition_operand = int_of_string condition_operand in
  let condition = fun x -> condition_op x condition_operand in
  let () = assert (parts = []) in
  (target_reg, operation), (condition_reg, condition)

let parse_rev_instructions lines =
  List.rev_map parse_instruction lines


let solver1 instructions =
  let registers = RegisterMap.fold_exe RegisterMap.empty instructions in
  RegisterMap.max_value registers;;

let my_input_file = open_in my_input_filename in
let my_input = rev_input_lines my_input_file in
let my_instructions = parse_rev_instructions my_input in
let () = close_in my_input_file in
let result1 = solver1 my_instructions in
let () = print_int result1 in
(* let () = print_newline () in
let result2 = solver2 my_input in
let () = print_int result2 in *)
print_newline ();;
