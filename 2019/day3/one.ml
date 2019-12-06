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

(* let find_boundaries lines =
 *   let minx = ref 0 in
 *   let maxx = ref 0 in
 *   let miny = ref 0 in
 *   let maxy = ref 0 in
 *   let process node =
 *     match split node with
 *     | 'U', value -> maxy := !maxy + value
 *     | _ -> ()
 *   in
 *   List.map process String.split_on_char ',' *)

let walk node origin =
  let path = [] in
  let x = origin.[0] in
  let y = origin.[1] in
  match node with
  | 'U', value -> for i = y to (y + value) do
                    if unseen [x, y + i] path
                    then [x, y + i]::path
                  done
  | 'D', value -> for i = y to (y - value) do
                    if unseen [x, y - i] path
                    then [x, y - i]::path
                  done
  | 'L', value -> for i = x to (x - value) do
                    if unseen [x - i, y] path
                    then [x - i, y]::path
                  done
  | 'R', value -> for i = x to (x + value) do
                    if unseen [x + i, y] path
                    then [x + i, y]::path
                  done
  | _ -> Exit

let draw line =
  let x = ref 0 in
  let y = ref 0 in
  let path = [] in
  let rec work node nodes =
    match node with
    | [] -> path
    | [node] -> (walk (split node) [0,0])::path
    | hd :: rest -> work hd rest
  in
  work [] String.split_on_char ',' line

let wrangle_lines lines =
  

let () =
  let lines = In_channel.read_lines "test" in
  let dimensions = find_boundaries lines in
  Array.iter (Printf.printf "Result: %s\n") (Array.of_list lines)
