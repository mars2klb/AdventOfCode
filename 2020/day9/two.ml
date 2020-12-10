let frobulate target data =
  let rec iterate front back =
    (* print_endline ("array length [" ^ string_of_int target ^ "]: " ^ string_of_int (Array.length data) ^ "  front:" ^ string_of_int front ^ "  back: " ^ string_of_int back); *)
    let numbers = Array.sub data front (back - front) in
    let total = Array.fold_left (fun acc x -> acc + x) 0 numbers in
    (* print_endline ("  [" ^ String.concat "|" (List.map string_of_int (Array.to_list numbers)) ^ "]");
     * print_endline ("total: " ^ string_of_int total); *)
    match total with
    | x when x = 0 -> iterate front (back + 1)
    | x when x < target -> iterate front (back + 1)
    | x when x > target -> iterate (front + 1) back
    | x when x = target -> let numlist = Array.to_list numbers in
                           let lower = List.fold_left (fun acc x -> min acc x) max_int numlist in
                           let upper = List.fold_left (fun acc x -> max acc x) 0 numlist in
                           lower + upper
    | _ -> raise (Failure "froblutation fault")
  in
  iterate 0 0

let run () =
  let data = Lib.In_channel.read_lines "day9/input"
           |> List.map int_of_string
           |> Array.of_list in
  let baddie = One.run () in
  string_of_int (frobulate (int_of_string baddie) data)
