exception Found of int

let find_my seats =
  let offset = List.hd seats in
  List.iteri (fun i x -> if i + offset != x then raise (Found (x - 1))) seats

let () =
  let data = In_channel.read_lines "input" in
  let seats = List.fast_sort compare (List.map (fun x -> One.handle x) data) in
  try
    find_my seats;
    raise (Failure "not found")
  with Found x -> print_endline ("Part 2: my seat: " ^ string_of_int x)
