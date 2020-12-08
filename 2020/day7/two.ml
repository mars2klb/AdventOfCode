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
        try
          let bags = Hashtbl.find table outer in
          Hashtbl.remove table outer;
          Hashtbl.add table outer (bags @ [entry])
        with Not_found -> Hashtbl.add table outer [entry]
    in
    extract (String.trim (Str.string_after line (Str.match_end ())))

let contents bag =
  try
    let _, b = bag in
    Hashtbl.find table b
  with Not_found -> []

let rec descend bags =
  List.map (fun x -> contents x) bags

let () =
  let data = In_channel.read_lines "test" in
  List.iter (fun x -> parse x) data;
  let rec iterate bags =
    let newbags = descend bags in
    print_endline ("NEWBAGS: " ^ String.concat "|" (List.map (fun (_, x) -> x) newbags));
    if List.length newbags = 0
    then bags
    else (iterate newbags) @ bags
  in
  let bags = iterate [(0, "shiny gold")] in
  List.iter (fun (x, y) -> print_endline (" " ^ string_of_int x ^ ":" ^ y)) bags
  (* print_endline ("Part 2: found " ^ string_of_int (bagcount - 1)) *)
