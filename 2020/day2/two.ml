let is_valid entry =
  let first, second, letter, password = One.parse entry in
  match (Char.escaped password.[first - 1 ] = letter, Char.escaped password.[second - 1] = letter) with
  | false, true
    | true, false -> true
  | _, _ -> false

let run () =
  let data = Lib.In_channel.read_lines "day2/input" in
  let good = List.filter is_valid data in
  string_of_int (List.length good) ^ " good passwords"
