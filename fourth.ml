let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let rev_input_lines chan =
  let rec aux acc =
    try
      aux ((input_line chan) :: acc)
    with End_of_file -> acc
  in aux [];;

let any_duplicates l = match List.sort compare l with
  | [] -> false
  | h :: t ->
      try
        let aux last current =
          if last = current then
            raise_notrace Exit
          else
            current
        in let _ = List.fold_left aux h t in false
      with Exit -> true;;


let solver1 lines =
  let aux acc line =
    let words = String.split_on_char ' ' line in
    if any_duplicates words then acc else acc + 1
  in List.fold_left aux 0 lines;;

let () = assert (solver1 ["aa bb cc dd ee"] = 1);
         assert (solver1 ["aa bb cc dd aa"] = 0);
         assert (solver1 ["aa bb cc dd aaa"] = 1);
         assert (solver1 ["aa bb cc dd ee"; "aa bb cc dd aa"; "aa bb cc dd aaa"] = 2);;

let fold_string_left func acc str =
  let len = String.length str in
  let rec aux a i =
    if i < len then
      aux (func a (String.get str i)) (i + 1)
    else
      a
  in
  aux acc 0;;

let rev_chars_of_string s =
  fold_string_left (fun a c -> c :: a) [] s;;

let solver2 lines =
  let aux acc line =
    let words = String.split_on_char ' ' line in
    let exploded = List.map (fun word -> List.sort compare (rev_chars_of_string word)) words in
    if any_duplicates exploded then acc else acc + 1
  in List.fold_left aux 0 lines;;

let () = assert (solver2 ["abcde fghij"] = 1);
         assert (solver2 ["abcde xyz ecdab"] = 0);
         assert (solver2 ["a ab abc abd abf abj"] = 1);
         assert (solver2 ["iiii oiii ooii oooi oooo"] = 1);
         assert (solver2 ["oiii ioii iioi iiio"] = 0);;

let my_input_file = open_in my_input_filename in
let my_input = rev_input_lines my_input_file in
let () = close_in my_input_file in
let result1 = solver1 my_input in
let () = print_int result1 in
let () = print_newline () in
let result2 = solver2 my_input in
let () = print_int result2 in
print_newline ();;
