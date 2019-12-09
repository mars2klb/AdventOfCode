open One

let count_steps target nodes =
  let steps = ref 0 in
  let rec count' n =
    match n with
    | [] -> !steps
    | hd :: rest -> (* Printf.printf "%d,%d(%d)%d,%d " hd.x hd.y !steps ((List.hd rest).x) ((List.hd rest).y); *)
                    if hd = target
                    then !steps
                    else
                      (if hd <> List.hd rest
                       then steps := !steps + 1;
                       count' rest)
  in
  count' nodes

let count target nodes =
  let first = List.hd nodes in
  let second = List.tl nodes
               |> List.hd in
  (count_steps target (List.rev first)) + (count_steps target (List.rev second))

let () =
  let lines = In_channel.read_lines "input" in
  let nodes = ref [] in
  let rec measure lines =
    match lines with
    | [] -> ()
    | hd :: rest -> nodes := (draw hd) :: !nodes; measure rest
  in
  measure lines;
  let ints = intersections !nodes in
  let distance_map = ref [] in
  let rec distances ints =
    match ints with
    | [] -> ()
    | hd :: rest -> distance_map := ((manhattan_distance hd), hd) :: !distance_map; distances rest
  in
  distances ints;
  let steps = List.map (fun t -> count t !nodes) ints in
  Printf.printf "result: %d\n" (closest 10000000 steps)
