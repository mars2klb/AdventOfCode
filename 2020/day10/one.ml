let churn joltages =
  let by_one = ref 0 in
  let by_three = ref 1 in
  let rec walk current rest =
    match rest with
    | hd :: tl -> (match hd - current with
                   | 3 -> by_three := !by_three + 1; walk hd tl
                   | 1 -> by_one := !by_one + 1; walk hd tl
                   | _ -> walk current tl)
    | [] -> !by_one * !by_three
  in
  walk 0 joltages

let run () =
  let joltages = Lib.In_channel.read_lines "day10/input"
                 |> List.map int_of_string
                 |> List.sort compare in
  string_of_int (churn ([0] @ joltages))
