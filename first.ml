let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let fold_string_left func acc str =
  (** Execute a function on every char in string from left to right *)
  let len = String.length str in
  let rec aux a i =
    if i < len then
      aux (func a (String.get str i)) (i + 1)
    else
      a
  in
  aux acc 0;;

let int_of_numeric_char c =
  let code = Char.code c in
  let () = assert (48 <= code && code <= 57) in
  code - 48;;

let rev_chars_of_string s =
  (** Explode a string into char list *)
  fold_string_left (fun a c -> c :: a) [] s;;

let digits_of_string s =
  List.rev_map int_of_numeric_char (rev_chars_of_string s);;

let solver digits compared_digits =
  let aux acc left_digit right_digit =
    if left_digit = right_digit then
      acc + left_digit
    else
      acc
  in List.fold_left2 aux 0 digits compared_digits;;

let cycle_list l n =
  let rec aux a i (h :: t as ls) =
    if i < n then
      aux (h :: a) (i + 1) t
    else
      (a, ls)
  in let (rr, l) = aux [] 0 l in
  let r = List.rev rr in
  l @ r;;

let cycle_list_one l =
  cycle_list l 1;;

let cycle_list_half l =
  cycle_list l ((List.length l) / 2);;

let solver1 l =
  solver l (cycle_list_one l);;

let solver2 l =
  solver l (cycle_list_half l);;

let () = assert (solver1 [1;1;2;2] = 3);
         assert (solver1 [1;1;1;1] = 4);
         assert (solver1 [1;2;3;4] = 0);
         assert (solver1 [9;1;2;1;2;1;2;9] = 9);
         assert (solver2 [1;2;1;2] = 6);
         assert (solver2 [1;2;2;1] = 0);
         assert (solver2 [1;2;3;4;2;5] = 4);
         assert (solver2 [1;2;3;1;2;3] = 12);
         assert (solver2 [1;2;1;3;1;4;1;5] = 4);;

let print_list func l =
  print_string "[";
  let rec aux = function
    | [] -> print_string "]"
    | x1 :: [] -> func x1; aux []
    | x1 :: ls -> func x1; print_string "; "; aux ls
  in aux l;;

let my_input_file = open_in my_input_filename in
let my_input = input_line my_input_file in
let () = close_in my_input_file in
let digits = digits_of_string my_input in
let result1 = solver1 digits in
let () = print_int result1 in
let () = print_newline () in
let result2 = solver2 digits in
let () = print_int result2 in
print_newline ();;
