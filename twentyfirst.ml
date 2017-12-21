let my_input_filename =
  "twentyfirst.input";;

let rev_input_lines chan =
  let rec aux acc =
    try
      aux ((input_line chan) :: acc)
    with End_of_file -> acc
  in aux [];;

exception Transformed of string list list

let split_four_into_twos p =
  if String.length (List.hd p) <> 4 then [p] else
  let l1 :: l2 :: l3 :: l4 :: [] = p in
  [[String.sub l1 0 2; String.sub l2 0 2];
   [String.sub l1 2 2; String.sub l2 2 2];
   [String.sub l3 0 2; String.sub l4 0 2];
   [String.sub l3 2 2; String.sub l4 2 2]]

let count_lit_of_line l =
  let counter = ref 0 in
  String.iter (function '#' -> counter := !counter + 1 | _ -> ()) l;
  !counter

let count_lit_of_square s =
  List.fold_left (+) 0 (List.rev_map count_lit_of_line s)

let count_lit_of_squares l =
  List.fold_left (fun a s -> a + count_lit_of_square s) 0 l

let parse_pattern p =
  String.split_on_char '/' p

let flip_string s =
  let len = String.length s in
  String.mapi (fun i _ -> s.[len - 1 - i]) s

let flip_pattern p =
  List.map flip_string p

let rotate_pattern p =
  let n = List.length p in
  List.mapi (fun i s -> (String.init n (fun j -> (List.nth p (n - 1 - j)).[i]))) p;;

let rotations_of_pattern p =
  let rot1 = rotate_pattern p in
  let rot2 = rotate_pattern rot1 in
  let rot3 = rotate_pattern rot2 in
  let () = assert(rotate_pattern rot3 = p) in
  [p; rot1; rot2; rot3]

let variate_pattern p =
  let pf = flip_pattern p in
  (rotations_of_pattern p) @ (rotations_of_pattern pf)

let parse_rule r =
  let original_pattern_str :: arrow :: new_pattern_str :: [] = String.split_on_char ' ' r in
  let original_pattern = parse_pattern original_pattern_str in
  let pattern_variations = variate_pattern original_pattern in
  let new_pattern = parse_pattern new_pattern_str in
  let new_pattern_split = split_four_into_twos new_pattern in
  fun sq ->
    if List.mem sq pattern_variations then
      raise_notrace (Transformed new_pattern_split)
    else
      sq;;

let parse_rules =
  List.rev_map parse_rule;;

let transform_square rules s =
  try
    [List.fold_left (fun sq f -> f sq) s rules]
  with Transformed (new_pl) ->
    new_pl;;

let transform_squares rules sqs =
  let transformer = transform_square rules in
  let transformed = List.rev_map transformer sqs in
  List.flatten transformed;;

let rec times count fn value =
  match count with
  | 0 -> value
  | _ -> times (count - 1) (fn) (fn value);;

let starting_squares = [parse_pattern ".#./..#/###"];;

let test_lines = ["../.# => ##./#../...";".#./..#/### => #..#/..../..../#..#"]
let test_rules = parse_rules test_lines
let test_second_iteration = times 2 (transform_squares test_rules) starting_squares
let test_result1 = count_lit_of_squares test_second_iteration
let () = assert (test_result1 = 12);;

let my_input_file = open_in my_input_filename
let my_input = rev_input_lines my_input_file
let () = close_in my_input_file
let my_rules = parse_rules my_input
let fifth_iteration = times 5 (transform_squares my_rules) starting_squares
let result1 = count_lit_of_squares fifth_iteration
let () = print_int result1
(* let () = print_newline () in
let result2 = solver2 my_input in
let () = print_int result2 in *)
print_newline ();;
