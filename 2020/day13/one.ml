let rec churn timestamp buses wait =
  let rec check runs =
    match runs with
    | hd :: [] -> if (timestamp mod hd) = 0
                  then (wait, hd)
                  else (0, 0)
    | hd :: tl -> if (timestamp mod hd) = 0
                  then (wait, hd)
                  else check tl
    | [] -> raise (Failure "in the weeds")
  in
  match (check buses) with
  | (0, 0) -> churn (timestamp + 1) buses (wait + 1)
  | x -> x

let run () =
  let data = Lib.In_channel.read_lines "day13/input" in
  let timestamp = int_of_string (String.trim (List.hd data)) in
  let buses = List.nth data 1
              |> String.split_on_char ','
              |> List.filter (fun x -> x <> "x")
              |> List.map int_of_string in
  let wait, bus = churn timestamp buses 0 in
  string_of_int (wait * bus) ^ "  route:" ^ string_of_int bus ^ "  wait:" ^ string_of_int wait
