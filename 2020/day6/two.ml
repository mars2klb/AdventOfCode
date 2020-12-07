let rec compact out unparsed =
  let rec crunch lines acc count =
    match lines with
    | hd :: tl when String.trim hd = "" -> compact (out @ [(count, acc)]) tl
    | hd :: tl -> crunch tl (acc ^ String.trim hd) (count + 1)
    | _ -> out @ [(count, acc)]
  in
  crunch unparsed "" 0

let tally votes =
  let voters, marks = votes in
  let tbl = Hashtbl.create 26 in
  List.iter (fun x ->
      try
        let count = Hashtbl.find tbl x in
        Hashtbl.add tbl x (count + 1)
      with Not_found -> Hashtbl.add tbl x 1)
    (One.explode marks);
  List.length (List.of_seq (Seq.filter (fun x -> x = voters) (Hashtbl.to_seq_values tbl)))

let () =
  let data = In_channel.read_lines "input"
             |> compact [] in
  print_endline ("Part 2: " ^ string_of_int (List.fold_left (fun acc x -> acc + (tally x)) 0 data))

