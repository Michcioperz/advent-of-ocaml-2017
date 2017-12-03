let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let square a = a * a

let solver1 i =
  if i = 1 then 0 else
  let rec aux last_mx depth =
    let current_mx = square (1 + depth * 2) in
    let layer_size = current_mx - last_mx in
    if i <= current_mx then
      let side_length = layer_size / 4 and side_center = depth in
      let position_on_side = (current_mx - i) mod side_length in
      let distance_from_center = abs (position_on_side - side_center) in
      depth + distance_from_center
    else
      aux current_mx (depth + 1)
  in aux 1 1

let () = assert (solver1 1 = 0);
         assert (solver1 12 = 3);
         assert (solver1 23 = 2);
         assert (solver1 1024 = 31);
         ();;

let my_input_file = open_in my_input_filename in
let my_input = int_of_string (input_line my_input_file) in
let () = close_in my_input_file in
let result1 = solver1 my_input in
let () = print_int result1 in
(* let () = print_newline () in
let result2 = (* solver2 *) in
let () = (* print_ *) result2 in *)
print_newline ();;
