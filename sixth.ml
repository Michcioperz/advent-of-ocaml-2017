let my_input_filename =
  String.lowercase_ascii __MODULE__ ^ ".input";;

let array_of_list len ls =
  let l = ref ls in
  Array.init len (fun _ -> let h :: t = !l in (l := t; h));;

module IntLists =
  struct
    type t = int list
    let compare = Pervasives.compare
  end;;

module IntListSet = Set.Make(IntLists);;

let list_of_array len arr =
  let rec loop i l =
    if i = 0 then
      l
    else
      let j = i - 1 in
      loop j (Array.get arr j :: l)
  in List.rev (loop len []);;

let max_array len arr =
  let max = ref 0 in
  let rec loop i =
    if i < len then
      let () = (if Array.get arr i > Array.get arr !max then
        max := i
      else ()) in
      loop (i + 1)
    else
      (!max)
  in loop 1;;

let solver1 ls =
  let len = List.length ls in
  let combos = ref IntListSet.empty in
  let arr = array_of_list len ls in
  let found = ref false in
  let combo = ref (list_of_array len arr) in
  while not !found do
    combos := IntListSet.add !combo !combos;
    let best_i = max_array len arr in
    let overhang = ref (Array.get arr best_i) in
    Array.set arr best_i 0;
    let i = ref best_i in
    while !overhang > 0 do
      i := !i + 1;
      if !i >= len then i := 0;
      overhang := !overhang - 1;
      Array.set arr !i ((Array.get arr !i) + 1)
    done;
    combo := list_of_array len arr;
    found := IntListSet.mem !combo !combos
  done;
  IntListSet.cardinal !combos

let () = assert(solver1 [0; 2; 7; 0] = 5);;

let solver2 ls =
  let len = List.length ls in
  let combos = ref IntListSet.empty in
  let arr = array_of_list len ls in
  let found = ref false in
  let combo = ref (list_of_array len arr) in
  while not !found do
    combos := IntListSet.add !combo !combos;
    let best_i = max_array len arr in
    let overhang = ref (Array.get arr best_i) in
    Array.set arr best_i 0;
    let i = ref best_i in
    while !overhang > 0 do
      i := !i + 1;
      if !i >= len then i := 0;
      overhang := !overhang - 1;
      Array.set arr !i ((Array.get arr !i) + 1)
    done;
    combo := list_of_array len arr;
    found := IntListSet.mem !combo !combos
  done;
  let reiterations = ref 0 in
  let our_goal = !combo in
  let reunited = ref false in
  while not !reunited do
    reiterations := !reiterations + 1;
    let best_i = max_array len arr in
    let overhang = ref (Array.get arr best_i) in
    Array.set arr best_i 0;
    let i = ref best_i in
    while !overhang > 0 do
      i := !i + 1;
      if !i >= len then i := 0;
      overhang := !overhang - 1;
      Array.set arr !i ((Array.get arr !i) + 1)
    done;
    let combo = list_of_array len arr in
    reunited := our_goal = combo
  done;
  !reiterations;;

let my_input_file = open_in my_input_filename in
let my_input_line = input_line my_input_file in
let () = close_in my_input_file in
let my_input_strs = String.split_on_char '\t' my_input_line in
let my_input = List.rev (List.rev_map int_of_string my_input_strs) in
let result1 = solver1 my_input in
let () = print_int result1 in
 let () = print_newline () in
let result2 = solver2 my_input in
let () = print_int result2 in
print_newline ();;
