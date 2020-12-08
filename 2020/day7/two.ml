let print_bag bag =
  let count, name = bag in
  string_of_int count ^ ":" ^ name

let table = Hashtbl.create 1024

let parse entry =
  let line = Str.global_replace (Str.regexp ",") "" entry in
  if Str.string_match One.outer_rgx line 0 then
    let outer = Str.matched_group 1 line in
    let rec extract rest =
      if Str.string_match One.inner_rgx rest 0 then
        let inner = Str.matched_group 2 rest in
        let inner_count = Str.matched_group 1 rest in
        let entry = (int_of_string inner_count, inner) in
        (try
           let bags = Hashtbl.find table outer in
           Hashtbl.remove table outer;
           Hashtbl.add table outer (bags @ [entry])
         with Not_found -> Hashtbl.add table outer [entry]);
        extract (String.trim (Str.string_after rest (Str.match_end ())))
    in
    extract (String.trim (Str.string_after line (Str.match_end ())))

let contents bag =
  try
    let _, b = bag in
    Hashtbl.find table b
  with Not_found -> []

let rec descend bag =
  let count, _ = bag in
  let bags = contents bag in
  let nums = List.map (fun x -> descend x) bags in
  (if count = 0 then 1 else count) * List.fold_left (fun acc x -> acc + x) 1 nums

let () =
  let data = In_channel.read_lines "input" in
  List.iter (fun x -> parse x) data;
  print_endline ("Part 2: you need " ^ (string_of_int ((descend (1, "shiny gold")) -1)) ^ " bags")
