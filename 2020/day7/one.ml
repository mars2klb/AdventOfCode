let outer_rgx = Str.regexp "^\\([a-z ]+\\).bags contain"
let inner_rgx = Str.regexp ".*\\([0-9]+\\).\\([a-z ]+\\) bag.?"
let table = Hashtbl.create 1024

let parse line =
  print_endline line;
  if Str.string_match outer_rgx line 0 then
    let outer = Str.matched_group 1 line in
    let rec extract offset =
      print_endline (" offset: " ^ string_of_int offset ^ ": " ^ String.sub line offset ((String.length line) - offset));
      if Str.string_match inner_rgx line offset then
        let inner = Str.matched_group 2 line in
        let inner_count = Str.matched_group 1 line in
        print_endline ("   outer:" ^ outer ^ "   inner:" ^ inner ^ "   count:" ^ inner_count);
        (try
          let bags = Hashtbl.find table outer in
          Hashtbl.add table outer (bags @ [(inner_count, inner)])
        with Not_found -> Hashtbl.add table outer []);
        (* extract ((String.length inner) + offset) *)
        extract (Str.match_end ())
      else
        print_endline ("unmatch")
    in
    extract (Str.match_end ())
    (* extract (String.length outer) *)

let () =
  let data = In_channel.read_lines "test" in
  List.iter (fun x -> parse x) data;
  Hashtbl.iter (fun k v -> print_endline ("key:" ^ k ^ " value:" ^ String.concat " "
                                                                     (List.map (fun (_, x) -> x) v))) table
