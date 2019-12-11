(* https://gitlab.com/unduthegun/adventofcode2019/blob/master/04/secure_container.ml *)

let input = (273025, 767253)

let range from until = List.init (until - from + 1) (fun x -> x + from)

let filter_increasing password =
  let rec loop prev = function
    | 6 -> true
    | i ->
       let curr = password.(i) in
       if curr < prev
       then false
       else loop curr (i + 1)
  in
  loop ~-1 0

let filter_repeats password =
  let rec loop prev = function
    | 6 -> false
    | i ->
       let curr = password.(i) in
       if curr = prev
       then true
       else loop curr (i + 1)
  in
  loop ~-1 0

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
    candidates |> Seq.filter (fun x -> filter_increasing x && filter_repeats x)
  in
  Printf.printf "Part 1: %u\n" (filtered |> List.of_seq |> List.length)
