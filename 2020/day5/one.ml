let fb_constraints = (0, 127)
let lr_constraints = (0, 7)

let explode s = List.init (String.length s) (String.get s)

let compute code lower upper constraints =
  let min, max = constraints in
  let rec iterate codes value last =
    let minv, maxv = value in
    let delta = int_of_float (ceil (float_of_int (maxv - minv) /. 2.0)) in
    match codes with
    | x :: tl when x = lower -> iterate tl (minv, maxv - delta) lower
    | x :: tl when x = upper -> iterate tl (minv + delta, maxv) upper
    | _ -> match last with
           | x when x = lower -> minv
           | x when x = upper -> maxv
           | _ as x -> raise (Failure ("bad code: " ^ Char.escaped x))
  in
  iterate (explode code) (min, max) 'x'

let handle seatcode =
  let fb = String.sub seatcode 0 7 in
  let lr = String.sub seatcode 7 3 in
  (compute fb 'F' 'B' fb_constraints) * 8 + (compute lr 'L' 'R' lr_constraints)
  
let () =
  let data = In_channel.read_lines "input" in
  let seats = List.map (fun x -> handle x) data in
  let max_seat = List.fold_left (fun acc x -> max acc x) 0 seats in
  print_endline ("Part 1: max seat id: " ^ string_of_int max_seat)
