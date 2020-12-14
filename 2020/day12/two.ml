let ship = ref (0, 0)

let mult x by = (if x = 0 then 1 else x) * by

let delta start finish =
  print_endline (" DELTA: start:" ^ Ahoy.dump_coord start ^ " finish:" ^ Ahoy.dump_coord finish);
  let sx, sy = start in
  let fx, fy = finish in
  ((abs (sx - fx)), (abs (sy - fy)))

let rotate_waypoint direction degrees from =
  let start = String.index Ahoy.cardinals !Ahoy.heading in
  Ahoy.rotate direction degrees;
  let wx, wy = (delta !ship from) in
  let sx, sy = !ship in
  let foo = match (start + (360 / degrees)) mod 4 with
    | 1 -> (sx + wx, sy + wy)
    | 2 -> (sx - wy, sy + wx)
    | 3 -> (sx - wx, sy - wy)
    | 0 -> (sx - wx, sy + wy)
    | _ -> raise (Failure "spinning out of control")
  in
  print_endline (" ROTATED: " ^ string_of_int degrees ^ "[" ^ string_of_int ((start + (360 / degrees)) mod 4) ^ "] " ^ Ahoy.dump_coord foo);
  foo

let move_ship toward from =
  let _, distance = toward in
  let cx, cy = !ship in
  let x, y = (delta !ship from) in
  let dx, dy = (mult x distance, mult y distance) in
  print_endline (" move ship from: " ^ Ahoy.dump_coord !ship);
  print_endline ("  waypoint " ^ Ahoy.dump_coord from);
  print_endline (" POSITION CHANGE: distance:" ^ string_of_int distance
                 ^ " delta:" ^ string_of_int x ^ ", " ^ string_of_int y);
  print_endline (" POSITION DX: " ^ string_of_int dx ^ ", " ^ string_of_int dy);
  let position = match !Ahoy.heading with
  | 'E' -> (cx + dx, cy + dy)
  | 'W' -> (cx - dx, cy + dy)
  | 'N' -> (cx + dx, cy + dy)
  | 'S' -> (cx - dx, cy - dy)
  | _ -> raise (Failure "ship lost")
  in
  ship := position;
  print_endline (" ship is at: " ^ Ahoy.dump_coord !ship);
  let px, py = position in
  print_endline ("  waypoint end:" ^ Ahoy.dump_coord (x + px, y + py));
  (x + px, y + py)

(* let move_ship toward from =
 *   let _, distance = toward in
 *   let cx, cy = !ship in
 *   let x, y = (delta !ship from) in
 *   let dx, dy = (cx + (mult x distance), cy + (mult y distance)) in
 *   print_endline (" move ship from: " ^ Ahoy.dump_coord !ship);
 *   print_endline ("  waypoint " ^ Ahoy.dump_coord from);
 *   print_endline (" POSITION CHANGE: distance:" ^ string_of_int distance
 *                  ^ " delta:" ^ string_of_int x ^ ", " ^ string_of_int y);
 *   print_endline (" POSITION DX: " ^ string_of_int dx ^ ", " ^ string_of_int dy);
 *   let position = match !Ahoy.heading with
 *     | 'E' -> (x + dx, y + dy)
 *     | 'W' -> (x - dx, y + dy)
 *     | 'N' -> (x + dx, y + dy)
 *     | 'S' -> (x - dx, y - dy)
 *     | _ -> raise (Failure "ship lost")
 *   in
 *   ship := position;
 *   print_endline (" ship is at: " ^ Ahoy.dump_coord !ship);
 *   position *)

let move toward from =
  let direction, distance = toward in
  let x, y = from in
  print_endline ("DIRECTION: " ^ Char.escaped direction ^ string_of_int distance
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
  let data = Lib.In_channel.read_lines "day12/test" in
  let points = Ahoy.chart data in
  let x, y = Ahoy.navigate move ([('E', 10); ('N', 1)] @ points) (0, 0) in
  "distance: " ^ string_of_int (x + y)
