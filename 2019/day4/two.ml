(* Notes:
 * 1. why ~-1
 * 2. what is function *)

open One

let filter_groups password =
  let rec loop prev matched = function
    | 6 -> matched = 1
    | i -> (
      let curr = password.(i) in
      match (curr = prev, matched) with
      | false, 1 -> true
      | true, _ -> loop curr (matched + 1) (i + 1)
      | false, _ -> loop curr 0 (i + 1))
  in
  loop ~-1 0 0

let () =
  let inputs =
    match input with from, until -> range from until in
  let candidates =
    inputs
    |> List.to_seq
    |> Seq.map (fun entry ->
           entry
           |> string_of_int
           |> String.to_seq
           |> Seq.map int_of_char
           |> Array.of_seq)
  in
  let filtered =
    candidates |> Seq.filter (fun x -> filter_increasing x && filter_repeats x && filter_groups x)
  in
  Printf.printf "Part 2: %u\n" (filtered |> List.of_seq |> List.length)
