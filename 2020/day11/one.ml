let dump seat =
  let center, left, right, up, down = seat in
  print_endline (" " ^ up " ");
  print_endline (left ^ center ^ right);
  print_endline (" " ^ down ^ " ")

let dump_seats seats =
  List.iter print_endline seats

let get_seat coord seats =
  let x, y = coord in
  try
    let line = List.nth seats y in
    line.[x]
  with
  | Failure _
  | Invalid_argument _ -> '.'

let get_neighbor center direction seats =
  let cx, cy = center in
  let dx, dy = direction in
  get_seat (cx + dx, cy + dy) seats

let rec occupied input output =
  match input with
  | '#' :: tl -> occupied tl (output + 1)
  | _ :: tl -> occupied tl output
  | [] -> output

let neighbors seat seats =
  let coords = [(-1, 0);  (* left *)
                (1, 0);   (* right *)
                (0, -1);  (* up *)
                (-1, -1); (* up/left *)
                (1, -1);  (* up/right *)
                (0, 1);   (* down *)
                (-1, 1);  (* down/left *)
                (1, 1)    (* down/right *)
               ] in
  let adj = List.map (fun x -> get_neighbor seat x seats) coords in
  match get_seat seat seats with
  | 'L' -> if (occupied adj 0) = 0 then '#' else 'L'
  | '#' -> if (occupied adj 0) >= 4 then 'L' else '#'
  | x -> x

let walk seats =
  List.mapi (fun x row -> String.mapi (fun y _ -> neighbors (x, y) seats) row) seats

let run () =
  let seats = Lib.In_channel.read_lines "day11/test" in
  dump_seats seats;
  let foo = walk seats in
  dump_seats foo;
  (* List.iter (fun x -> dump x) foo; *)
  "unimplemented"
