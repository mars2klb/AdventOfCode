let next current joltages =
  List.filter (fun x -> List.exists (fun x -> x = current + x) [3; 2; 1]) joltages

let churt joltages =
  let by_one = ref 0 in
  let by_three = ref 1 in
  let rec walk current rest =
    match rest with
    | hd :: tl -> (match List.hd (next current rest) with
                   | 3 -> by_three := !by_three + 1; walk (hd + 3) tl
                   | 1 -> by_one := !by_one + 1; walk (hd + 1) tl
                   | _ -> walk current tl)
    | [] -> !by_one * !by_three
  in
  walk 0 joltages

let run () =
  let joltages = Lib.In_channel.read_lines "day10/test"
                 |> List.map int_of_string
                 |> List.sort compare in
  churn joltages;
  "unimplemented"
