let explode s = List.init (String.length s) (String.get s)
let implode s =
  let result = ref "" in
  List.iter (fun x -> result := !result ^ Char.escaped x) s;
  String.trim !result

let rec compact out unparsed =
  let rec crunch lines acc =
    match lines with
    | hd :: tl when String.trim hd = "" -> compact (out @ [acc]) tl
    | hd :: tl -> crunch tl (acc ^ " " ^ hd)
    | _ -> out @ [acc]
  in
  crunch unparsed ""

let () =
  let data = In_channel.read_lines "input"
             |> compact []
             |> List.map (fun x -> List.sort_uniq compare (explode x))
             |> List.map (fun x -> implode x) in
  let count = List.fold_left (fun acc x -> acc + (String.length x)) 0 data in
  print_endline ("Part 1: " ^ string_of_int count)
