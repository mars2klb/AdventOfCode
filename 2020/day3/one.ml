let rec run lines index count by =
  match lines with
  | line :: rest ->
     if line.[index mod String.length line] = '#'
     then run rest (index + by) (count + 1) by
     else run rest (index + by) count by
  | [] -> count

let () =
  let data = In_channel.read_lines "input" in
  print_endline ("Part 1: found " ^ string_of_int (run data 0 0 3) ^ " trees")
