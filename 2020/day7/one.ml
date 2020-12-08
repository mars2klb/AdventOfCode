let outer_rgx = Str.regexp "^\\([a-z ]+\\) bags contain"
let inner_rgx = Str.regexp "\\([0-9]+\\) \\([a-z ]+\\) bag.?"

let parse entry table rtable =
  let line = Str.global_replace (Str.regexp ",") "" entry in
  if Str.string_match outer_rgx line 0 then
    let outer = Str.matched_group 1 line in
    let rec extract rest =
      if Str.string_match inner_rgx rest 0 then
        let inner = Str.matched_group 2 rest in
        let inner_count = Str.matched_group 1 rest in
        (try let _ = Hashtbl.find table outer in ()
         with Not_found -> Hashtbl.add table outer (int_of_string inner_count));
        (try
           let bags = Hashtbl.find rtable inner in
           Hashtbl.remove rtable inner;
           Hashtbl.add rtable inner (bags @ [outer])
         with Not_found -> Hashtbl.add rtable inner [outer]);
        extract (String.trim (Str.string_after rest (Str.match_end ())))
    in
    extract (String.trim (Str.string_after line (Str.match_end ())))

let expand bags rtable =
  let allbags = ref [] in
  List.iter (fun x ->
      try
        let baglist = Hashtbl.find rtable x in
        allbags := !allbags @ baglist
      with Not_found -> ()) bags;
  List.sort_uniq compare !allbags

let () =
  let data = In_channel.read_lines "input" in
  let table = Hashtbl.create 1024 in
  let rtable = Hashtbl.create 1024 in
  List.iter (fun x -> parse x table rtable) data;
  let rec iterate bags =
    let newbags = expand bags rtable in
    if List.length newbags = 0
    then bags
    else (iterate newbags) @ bags
  in
  let bags = List.sort_uniq compare (iterate ["shiny gold"]) in
  print_endline ("Part 1: found " ^ string_of_int ((List.length bags) - 1))
