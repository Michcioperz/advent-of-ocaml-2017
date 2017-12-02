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

let solver1 (first_digit :: other_digits as digits : int list) =
  let next_digits = List.rev (first_digit :: (List.rev other_digits)) in
  let aux acc left_digit right_digit =
    let () = Printf.printf "%d\t%d\n" left_digit right_digit in
    if left_digit = right_digit then
      acc + left_digit
    else
      acc
  in List.fold_left2 aux 0 digits next_digits;;

let () = assert (solver1 [1;1;2;2] = 3);
         assert (solver1 [1;1;1;1] = 4);
         assert (solver1 [1;2;3;4] = 0);
         assert (solver1 [9;1;2;1;2;1;2;9] = 9);;

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
let () = print_endline my_input in
let digits = digits_of_string my_input in
let () = print_list print_int digits in
let result1 = solver1 digits in
let () = print_int result1 in
print_newline ();;
