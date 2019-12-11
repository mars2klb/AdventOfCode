let run stream =
  let rec compute head = match stream.(head) with
    | 99 -> stream
    | 1 -> let lhs = stream.(head + 1) in
           let rhs = stream.(head + 2) in
           let dst = stream.(head + 3) in
           stream.(dst) <- stream.(lhs) + stream.(rhs);
           compute (head + 4)
    | 2 -> let lhs = stream.(head + 1) in
           let rhs = stream.(head + 2) in
           let dst = stream.(head + 3) in
           stream.(dst) <- stream.(lhs) * stream.(rhs);
           compute (head + 4)
    | 3 -> let arg = stream.(head + 1) in
           compute (head + 3)
    | _ -> raise Exit
  in
  compute 0

let () =
  let data = In_channel.read_lines "input"
             |> List.hd
             |> String.split_on_char ','
             |> List.map int_of_string
  in
  Printf.printf "Result: %d\n" (run (Array.of_list data)).(0)
