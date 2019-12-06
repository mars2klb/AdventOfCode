let split node =
  let opcode = node.[0] in
  let value = String.split_on_char opcode node
              |> List.tl
              |> List.hd
              |> int_of_string
  in
  opcode, value

let rec unseen node list =
  match list with
  | [] -> true
  | hd :: rest -> if hd = node
                  then false
                  else unseen node rest

let walk node origin =
  let path = ref [] in
  let x, y = origin in
  match node with
  | 'U', value -> for i = y to (y + value) do
                    if unseen [x, y + i] !path
                    then path := [x, y + i] :: !path
                  done
  | 'D', value -> for i = y to (y - value) do
                    if unseen [x, y - i] !path
                    then path := [x, y - i] :: !path
                  done
  | 'L', value -> for i = x to (x - value) do
                    if unseen [x - i, y] !path
                    then path:= [x - i, y] :: !path
                  done
  | 'R', value -> for i = x to (x + value) do
                    if unseen [x + i, y] !path
                    then path := [x + i, y] :: !path
                  done
  | _ -> ()

let draw line =
  let path = ref [] in
  let rec work node nodes =
    match nodes with
    | [] -> !path
    | [node] -> path := (walk (split node) (0,0)) :: !path; !path
    | hd :: rest -> work hd rest
  in
  work "" (String.split_on_char ',' line)

let () =
  let lines = In_channel.read_lines "test" in
  let dimensions = List.map draw lines in
  let print_line line =
    List.iter (fun x -> Printf.printf "%c, %c\n" x.[0] x.[1]) line
  in
  List.map print_line dimensions
  (* for i = 0 to List.length dimensions do
   *   let line = dimensions.(i) in
   *   Printf.printf "list length: %d\n" (List.length line)
   *   (\* for j = 0 to List.length dimensions.[i] do
   *    *   Printf.printf "-> %d,%d\n" dimensions.[i].[j]
   *    * done *\)
   * done
   * (\* List.map (Printf.printf "Result: %d\n") dimensions.[0] *\) *)
