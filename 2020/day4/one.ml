let valid_field field passport =
  let id, rgx = field in
  Str.string_match (Str.regexp (".*" ^ id ^ ":" ^ rgx)) passport 0

let rec compact out unparsed =
  let rec crunch lines acc =
    match lines with
    | hd :: tl when String.trim hd = "" -> compact (out @ [acc]) tl
    | hd :: tl -> crunch tl (acc ^ " " ^ hd)
    | _ -> out @ [acc]
  in
  crunch unparsed ""

let () =
  let data = In_channel.read_lines "input"
             |> compact [] in
  let fields = [("byr", "[0-9]+");
                ("iyr", "[0-9]+");
                ("eyr", "[0-9]+");
                ("hgt", "[0-9]+[a-z]*");
                ("hcl", "[#0-9a-z]+");
                ("ecl", "[#0-9a-z]+");
                (* ("cid", "[0-9]+"); *) 
                ("pid", "[#0-9a-z]+")] in
  let valid_passports = List.filter
                          (fun x -> List.fold_left (fun acc y -> acc && valid_field y x) true fields)
                          data in
  print_endline ("Part 1: found " ^ string_of_int (List.length valid_passports) ^ " valid passports")
