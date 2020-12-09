let lessThan2020 l r = l + r < 2020

let find_less_than var data =
  let results = ref [] in
  let rec look_for item within =
    match within with
    | hd :: tl ->
        let matches = List.filter (fun x -> x != hd && lessThan2020 x hd) data in
        List.iter (fun x -> results := !results @ [(x, hd)]) matches ;
        look_for item tl
    | [] -> !results in
  look_for var data

let rec solve tup data =
  let l, r = tup in
  match data with
  | hd :: _ when hd + l + r = 2020 -> hd * l * r
  | _ :: tl -> solve tup tl
  | [] -> 0

let run () =
  let data = Lib.In_channel.read_lines "day1/input" |> List.map int_of_string in
  let potentials = List.flatten (List.map (fun x -> find_less_than x data) data) in
  let answer = List.filter (fun x -> x > 0) (List.map (fun x -> solve x data) potentials) in
  string_of_int (List.hd answer)
