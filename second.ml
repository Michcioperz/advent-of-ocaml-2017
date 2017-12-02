let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let rev_input_lines chan =
  let rec aux acc =
    try
      aux ((input_line chan) :: acc)
    with End_of_file -> acc
  in aux [];;

let rev_numbers_of_line line =
  let words = String.split_on_char '\t' line in
  List.rev_map int_of_string words

let min_max_of_list (h :: t) =
  let aux (mn, mx) x =
    min mn x, max mx x
  in List.fold_left aux (h, h) t

let solver1 lines =
  let minmaxes = List.rev_map min_max_of_list lines in
  let diffs = List.rev_map (fun (mn, mx) -> mx - mn) minmaxes in
  List.fold_left (fun a x -> a + x) 0 diffs

let () = assert (solver1 [[5; 1; 9; 5]; [7; 5; 3]; [2; 4; 6; 8]] = 18);;

let is_some = function
  | Some _ -> true
  | None -> false

let unwrap l =
  let rec aux acc = function
    | [] -> acc
    | None :: t -> aux acc t
    | Some x :: t -> aux (x :: acc) t
  in aux [] l

let outer_product l1 l2 =
  unwrap
  (List.flatten
    (List.mapi 
      (fun i x -> List.mapi
        (fun j y -> if i = j then None else Some (x, y)) l2)
      l1))

let divisible_of_line line =
  let is_divisible (x, y) = x mod y = 0 || y mod x = 0 in
  let (a, b) = List.find is_divisible (outer_product line line) in
  (max a b) / (min a b)

let solver2 lines =
  let divisibles = List.map divisible_of_line lines in
  List.fold_left (fun a x -> a + x) 0 divisibles

let () = assert (solver2 [[5;9;2;8];[9;4;7;3];[3;8;6;5]] = 9);;

let my_input_file = open_in my_input_filename in
let my_input = rev_input_lines my_input_file in
let () = close_in my_input_file in
let spreadsheet = List.map rev_numbers_of_line my_input in
let result1 = solver1 spreadsheet in
let () = print_int result1 in
let () = print_newline () in
let result2 = solver2 spreadsheet in
let () = print_int result2 in
print_newline ();;
