let ship = ref (0, 0)

let mult x by = (if x = 0 then 1 else x) * by

let turn_left pos =
  let x, y = pos in (-y, x)

let turn_right pos =
  let x, y = pos in (y, -x)

let rotate_waypoint direction degrees from =
  print_endline ("Rotate from " ^ Char.escaped direction ^ " " ^ string_of_int degrees ^ " " ^ Ahoy.dump_coord from);
  let x, y = from in
  let delta = (degrees / 90) mod 4 in
  if delta mod 2 = 0
  then (-x, -y)
  else match direction with
       | 'R' -> if delta = 1 then turn_right from else turn_left from
       | 'L' -> if delta = 1 then turn_left from else turn_right from
       | _ -> raise (Failure "bad waypoint rotation")
  
let move_ship toward from =
  let _, distance = toward in
  let cx, cy = !ship in
  let x, y = from in
  let dx, dy = (mult x distance, mult y distance) in
  print_endline (" move ship from: " ^ Ahoy.dump_coord !ship);
  print_endline ("  waypoint " ^ Ahoy.dump_coord from);
  print_endline (" POSITION CHANGE: distance:" ^ string_of_int distance
                 ^ " delta:" ^ string_of_int x ^ ", " ^ string_of_int y);
  print_endline (" POSITION DX: " ^ string_of_int dx ^ ", " ^ string_of_int dy);
  let position = (cx + dx, cy + dy) in ship := position;
  print_endline (" ship is at: " ^ Ahoy.dump_coord !ship);
  print_endline ("  waypoint end:" ^ Ahoy.dump_coord from);
  from

let move toward from =
  let direction, distance = toward in
  let x, y = from in
  print_endline ("MOVE: " ^ Char.escaped direction ^ string_of_int distance
                 ^"  [waypoint: " ^ string_of_int x ^ ", " ^ string_of_int y ^ "]");
  match direction with
  | 'R' | 'L' -> rotate_waypoint direction distance from
  | 'E' -> (x + distance, y)
  | 'W' -> (x - distance, y)
  | 'N' -> (x, y + distance)
  | 'S' -> (x, y - distance)
  | 'F' -> move_ship (!Ahoy.heading, distance) from
  | _ -> raise (Failure "bad move")

let run () =
  Ahoy.heading := 'E';
  let data = Lib.In_channel.read_lines "day12/input" in
  let points = Ahoy.chart data in
  ignore (Ahoy.navigate move ([('E', 10); ('N', 1)] @ points) (0, 0));
  let x, y = !ship in
  "distance: " ^ string_of_int (abs x + abs y)
