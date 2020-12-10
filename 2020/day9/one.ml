let dump factor =
  let x, y = factor in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let get_window numbers offset preamble =
  Array.sub numbers offset preamble

let generate_factors number window =
  Array.map (fun x -> (x, number - x)) window

let find factor window =
  let l, r = factor in
  (* there's probably a more succinct way to do this *)
  Array.exists (fun x -> x = l) window && Array.exists (fun x -> x = r) window

let validate seed factors window =
  match (Array.fold_left (fun acc x -> acc || find x window) false factors) with
  | x when x = true -> ()
  | _ -> raise (Failure (string_of_int seed))

let run () =
  let preamble = 25 in
  let data = Lib.In_channel.read_lines "day9/input"
             |> List.map int_of_string
             |> Array.of_list in
  try
    Array.iteri (fun idx x ->
        if idx >= preamble then
          let window = get_window data (idx - preamble) preamble in
          validate x (generate_factors x window) window) data;
    "failed to find anything"
  with Failure x -> x
