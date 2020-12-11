exception Out_of_bounds

let dump seat =
  let x, y = seat in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let dump_seats seats = String.concat "\n" seats

let get_seat coord seats =
  let x, y = coord in
  try
    let line = List.nth seats y in
    line.[x]
  with Failure _ | Invalid_argument _ -> raise Out_of_bounds

let seat_coord start direction =
  let x, y = start in
  let dx, dy = direction in
  (x + dx, y + dy)
  
let get_neighbor center direction seats =
  try
    get_seat (seat_coord center direction) seats
  with Out_of_bounds -> '.'

let rec get_line_of_sight_neighbor center direction seats =
  let seat = get_neighbor center direction seats in
  match seat with
  | '.' -> get_line_of_sight_neighbor (seat_coord center direction) direction seats
  | s -> s

let rec occupied input output =
  match input with '#' :: tl -> occupied tl (output + 1) | _ :: tl -> occupied tl output | [] -> output

let neighbors tolerance seat seats =
  let coords =
    (* left, right, up, up/left, up/right, down, down/left, down/right *)
    [(-1, 0); (1, 0); (0, -1); (-1, -1); (1, -1); (0, 1); (-1, 1); (1, 1)] in
  let adj = List.map (fun x -> get_neighbor seat x seats) coords in
  let mark =
    try
      get_seat seat seats
    with Out_of_bounds -> '.' in
  (* match get_seat seat seats with *)
  match mark with
  | 'L' -> if occupied adj 0 = 0 then '#' else 'L'
  | '#' -> if occupied adj 0 >= tolerance then 'L' else '#'
  | x -> x

let walk tolerance seats = List.mapi (fun y row -> String.mapi (fun x _ -> neighbors tolerance (x, y) seats) row) seats
let tally seats = List.fold_left (fun acc row -> acc + occupied (List.of_seq (String.to_seq row)) 0) 0 seats

let rec churn ?tolerance:(tolerance=4) seats count =
  let churned = walk tolerance seats in
  if List.for_all2 (fun a b -> String.equal a b) seats churned then (
    (* print_endline ("iterations: " ^ string_of_int count) ;
     * print_endline (dump_seats churned) ; *)
    tally churned )
  else churn ~tolerance:tolerance churned (count + 1)

let run () =
  let seats = Lib.In_channel.read_lines "day11/input" in
  string_of_int (churn seats 0)
