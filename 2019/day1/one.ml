let compute_fuel mass =
  let f = (mass / 3) - 2 in
  if f < 0 then 0 else f

let () =
  let lines = In_channel.read_lines "one.input"
              |> List.map int_of_string in
  let total_fuel = List.map compute_fuel lines
                   |> List.fold_left (+) 0 in
  Printf.printf "Exercise 1: %d\n" total_fuel
