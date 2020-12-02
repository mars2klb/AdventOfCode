let equal2020 l r = (l + r = 2020)

let find_equal var data =
  let rec look_for item within =
  match within with
  | hd :: tl ->
     (let rhs = List.find_opt (fun x -> x != hd && equal2020 x hd) data in
      match rhs with
      | Some value -> (hd, value)
      | None -> look_for item tl)
  | [] -> raise (Failure "not found")
  in
  look_for var data

let () =
  let data = In_channel.read_lines "input"
           |> List.map int_of_string in
  match (find_equal (List.hd data) data) with
  | (match1, match2) when (match1 * match2 != 0) ->  print_endline ("Part 1: " ^ string_of_int match1 ^ " <-> " ^ string_of_int match2 ^ " :: " ^ string_of_int (match1 * match2))
  | _ -> raise (Failure "not found")
