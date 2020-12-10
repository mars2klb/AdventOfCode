let dump lst = String.concat "|" (List.map string_of_int lst)

let get offset from =
  try
    List.nth from offset
  with
  | Failure _ -> 0
  | Invalid_argument _ -> 0

let has joltage from =
  List.exists (fun x -> x = joltage) from

let rec churn input output =
  match input with
  | hd :: tl -> let variants = List.length(List.filter (fun x -> has (hd + x) tl) [1; 2; 3]) in
                churn tl (output @ [if variants = 0 then 1 else (if variants > 1 then 2 else 1)])
  | [] -> output

let rec prune input output =
  match input with
  | 2 :: 2 :: 2 :: tl -> prune tl (output @ [7])
  | hd :: tl -> prune tl (output @ [hd])
  | [] -> output

let run () =
  let joltages = Lib.In_channel.read_lines "day10/input"
                 |> List.map int_of_string
                 |> List.sort compare in
  let padded = [0] @ joltages @ [(List.hd (List.rev joltages)) + 3] in
  let permutations = churn padded [] in
  let pruned = prune permutations [] in
  let total = List.fold_left ( * ) 1 pruned in
  "total: " ^ string_of_int total
