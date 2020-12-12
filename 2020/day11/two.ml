exception Out_of_bounds of char

let get_seat coord seats =
  let x, y = coord in
  try
    let line = List.nth seats y in
    line.[x]
  with Failure _ | Invalid_argument _ -> raise (Out_of_bounds 'X')

let seat_coord start direction =
  let x, y = start in
  let dx, dy = direction in
  (x + dx, y + dy)
  
let get_neighbor center direction seats =
    get_seat (seat_coord center direction) seats

let rec get_line_of_sight_neighbor center direction seats =
  let seat = get_neighbor center direction seats in
  match seat with
  | '.' -> get_line_of_sight_neighbor (seat_coord center direction) direction seats
  | s -> s

let rec occupied input count =
  match input with
  | '#' :: tl -> occupied tl (count + 1)
  | _ :: tl -> occupied tl count
  | [] -> count

let neighbors tolerance finder seat seats =
  let coords =
    (* left, right, up, up/left, up/right, down, down/left, down/right *)
    [(-1, 0); (1, 0); (0, -1); (-1, -1); (1, -1); (0, 1); (-1, 1); (1, 1)] in
  let adjacents = List.map (fun x -> try finder seat x seats with Out_of_bounds x -> x) coords in
  let mark =
    try
      get_seat seat seats
    with Out_of_bounds x -> x in
  match mark with
  | 'L' -> if occupied adjacents 0 = 0 then '#' else 'L'
  | '#' -> if occupied adjacents 0 >= tolerance then 'L' else '#'
  | x -> x

let walk tolerance finder seats = List.mapi (fun y row -> String.mapi (fun x _ -> neighbors tolerance finder (x, y) seats) row) seats
let tally seats = List.fold_left (fun acc row -> acc + occupied (List.of_seq (String.to_seq row)) 0) 0 seats

(* This passing of the filter is busted *)
let rec churn ?tolerance:(tolerance=5) ?finder:(finder=get_line_of_sight_neighbor) seats count =
  let churned = walk tolerance finder seats in
  if List.for_all2 (fun a b -> String.equal a b) seats churned then tally churned
  else churn ~tolerance:tolerance churned (count + 1)

let run () =
  let seats = Lib.In_channel.read_lines "day11/input" in
  string_of_int (churn seats 0)

