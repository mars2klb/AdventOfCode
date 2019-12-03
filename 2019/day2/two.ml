let run stream noun verb =
  stream.(1) <- noun; stream.(2) <- verb; One.run stream

let work sending =
  for noun = 0 to 99 do
    for verb = 0 to 99 do
      let output = run (Array.copy sending) noun verb in
      if output.(0) = 19690720
      then Printf.printf "Part 2: %d\n" (100 * noun + verb)
    done
  done

let () =
  let data = In_channel.read_lines "input"
             |> List.hd
             |> String.split_on_char ','
             |> List.map int_of_string
  in
  work (Array.of_list data)
