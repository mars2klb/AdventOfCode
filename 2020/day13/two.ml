let dump route =
  let idx, bus = route in
  "(" ^ string_of_int idx ^ ":" ^ string_of_int bus ^ ")"

let valid timestamp bus =
  let idx, busnum = bus in
  let good = (timestamp + idx) mod busnum = 0 in
  if good then good else raise (Failure "short circuit")

let rec churn timestamp buses =
  if
    try
      List.fold_left (fun acc x -> acc && valid timestamp x) true buses
    with Failure _ -> false
  then timestamp
  else
    let _, incrementer = (List.hd buses) in
    churn (timestamp + incrementer) buses
  
let run () =
  let data = Lib.In_channel.read_lines "day13/input" in
  let routes = List.nth data 1
               |> String.split_on_char ','
               |> List.mapi (fun idx x -> (idx, if x = "x" then 0 else int_of_string x))
               |> List.filter (fun (_, x) -> x != 0)
               |> List.sort (fun (_, a) (_, b) -> b - a) in
  (* List.iter (fun x -> print_endline (dump x)) routes; *)
  let idx, bus = List.hd routes in
  string_of_int (churn (bus - idx) routes)
