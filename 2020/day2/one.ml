let parse entry =
    if Str.string_match (Str.regexp "^\\([0-9]+\\)-\\([0-9]+\\) \\([a-z]\\): \\([a-z]+\\)$") entry 0
  then
    (int_of_string (Str.matched_group 1 entry),
     int_of_string (Str.matched_group 2 entry),
     Str.matched_group 3 entry,
     Str.matched_group 4 entry)
  else
    raise (Failure ("failed to match: " ^ entry))

let is_valid entry =
  let atleast, atmost, letter, password = parse entry in
  let letters = String.of_seq (Seq.filter (fun x -> Char.escaped x = letter) (String.to_seq password)) in
  let letter_count = String.length letters in
  letter_count >= atleast && letter_count <= atmost

let run () =
  let data = Lib.In_channel.read_lines "day2/input" in
  let good = List.filter is_valid data in
  string_of_int (List.length good) ^ " good passwords"
