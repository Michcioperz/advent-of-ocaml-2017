let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let rev_input_ints chan =
  let rec aux acc =
    try
      aux (int_of_string (input_line chan) :: acc)
    with End_of_file -> acc
  in aux [];;

let rev_and_count ls =
  List.fold_left (fun (n, t) h -> (n + 1, h :: t)) (0, []) ls

let solver1 len arr =
  let rec loop jump_count pos =
    if pos < 0 || pos >= len then
      jump_count
    else
      let current_value = Array.get arr pos in
      let () = Array.set arr pos (current_value + 1) in
      loop (jump_count + 1) (pos + current_value)
  in loop 0 0

let () = assert (solver1 5 [|0; 3; 0; 1; -3|] = 5);;

let solver2 len arr =
  let rec loop jump_count pos =
    if pos < 0 || pos >= len then
      jump_count
    else
      let current_value = Array.get arr pos in
      let () = Array.set arr pos (if current_value >= 3 then current_value - 1 else current_value + 1) in
      loop (jump_count + 1) (pos + current_value)
  in loop 0 0

let () = assert (solver2 5 [|0; 3; 0; 1; -3|] = 10);;


let my_input_file = open_in my_input_filename in
let my_input_ls = rev_input_ints my_input_file in
let () = close_in my_input_file in
let my_length, my_inputs = rev_and_count my_input_ls in
let my_input_array1 = Array.init my_length (List.nth my_inputs) in
let result1 = solver1 my_length my_input_array1 in
let () = print_int result1 in
let () = print_newline () in
let my_input_array2 = Array.init my_length (List.nth my_inputs) in
let result2 = solver2 my_length my_input_array2 in
let () = print_int result2 in
print_newline ();;
