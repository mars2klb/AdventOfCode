let rec compute lines index count by =
  match lines with
  | line :: rest ->
     if line.[index mod String.length line] = '#'
     then compute rest (index + by) (count + 1) by
     else compute rest (index + by) count by
  | [] -> count

let run () =
  let data = Lib.In_channel.read_lines "day3/input" in
  string_of_int (compute data 0 0 3) ^ " trees"
