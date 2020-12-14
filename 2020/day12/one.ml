let rec move toward from =
  let direction, distance = toward in
  let x, y = from in
  match direction with
  | 'R' | 'L' -> Ahoy.rotate direction distance; from
  | 'E' -> (x + distance, y)
  | 'W' -> (x - distance, y)
  | 'N' -> (x, y + distance)
  | 'S' -> (x, y - distance)
  | 'F' -> move (!Ahoy.heading, distance) from
  | _ -> raise (Failure "bad move")

let run () =
  Ahoy.heading := 'E';
  let data = Lib.In_channel.read_lines "day12/input" in
  let points = Ahoy.chart data in
  let x, y = Ahoy.navigate move points (0, 0) in
  "distance: " ^ string_of_int (x + y)

