let compute_fuel' mass =
  let rec computate acc x =
    if x <= 0 then (acc - x)
    else
      let f = One.compute_fuel x in
      computate (acc + f) f
  in
  computate 0 mass

let () =
  let lines = In_channel.read_lines "one.input"
              |> List.map int_of_string in
  let total_fuel = List.map compute_fuel' lines
                   |> List.fold_left (+) 0 in
  Printf.printf "Exercise 2: %d\n" total_fuel
