let is_valid entry =
  let first, second, letter, password = One.parse entry in
  match (Char.escaped password.[first - 1 ] = letter, Char.escaped password.[second - 1] = letter) with
  | false, true
    | true, false -> true
  | _, _ -> false

let () =
  let data = In_channel.read_lines "input" in
  let good = List.filter is_valid data in
  print_endline ("Part 2: found " ^ string_of_int (List.length good) ^ " good passwords")
