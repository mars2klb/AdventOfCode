let valid_year prefix atleast atmost passport =
  if Str.string_match (Str.regexp (".*" ^ prefix ^ ":\\([0-9][0-9][0-9][0-9]\\)")) passport 0 then
    let year = Str.matched_group 1 passport |> int_of_string in
    year >= atleast && year <= atmost
  else
    false

let valid_height passport =
  if Str.string_match (Str.regexp ".*hgt:\\([0-9]+\\)\\(cm\\|in\\)") passport 0 then
    let height = Str.matched_group 1 passport |> int_of_string in
    let units = Str.matched_group 2 passport in
    match units with
    | "cm" -> height >= 150 && height <= 193
    | "in" -> height >= 59 && height <= 76
    | _ -> false
  else
    false

let valid_ecl constraints passport =
  if Str.string_match (Str.regexp ".*ecl:\\([a-z]+\\)") passport 0 then
    let color = Str.matched_group 1 passport in
    try List.exists (fun x -> x = color) constraints
    with _ -> false
  else
    false

let valid_hcl passport =
  Str.string_match (Str.regexp (".*hcl:\\(#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\)")) passport 0

let valid_pid passport =
  if Str.string_match (Str.regexp ".*pid:\\([0-9]+\\)") passport 0 then
    let pid = Str.matched_group 1 passport in
    String.length pid = 9
  else
    false

let () =
  let data = In_channel.read_lines "input"
             |> One.compact [] in
  let fields = [(valid_year "byr" 1920 2002);
                (valid_year "iyr" 2010 2020);
                (valid_year "eyr" 2020 2030);
                (valid_height);
                (valid_hcl);
                (valid_ecl ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]);
               (valid_pid)] in
  let valid_passports = List.filter
                          (fun x -> List.fold_left (fun acc y -> acc && y x) true fields)
                          data in
  print_endline ("Part 2: found " ^ string_of_int (List.length valid_passports) ^ " valid passports")
