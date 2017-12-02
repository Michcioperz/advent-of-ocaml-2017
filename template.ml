let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;



let my_input_file = open_in my_input_filename in
let my_input = (* input_ *) my_input_file in
let () = close_in my_input_file in
(* data parsing here *)
let result1 = (* solver1 *) in
let () = (* print_ *) result1 in
let () = print_newline () in
let result2 = (* solver2 *) in
let () = (* print_ *) result2 in
print_newline ();;
