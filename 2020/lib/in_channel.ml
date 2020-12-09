(* https://gist.github.com/anuragsoni/e8e184ac6dc7cad5d42f66eb03e74f95/raw/2a2ad3f153477513cf910e42f6e22384f810d19b/in_channel.ml *)
let read_lines name =
  let input_line_opt ic = try Some (input_line ic) with End_of_file -> None in
  let ic = open_in name in
  let rec aux acc =
    match input_line_opt ic with
    | None ->
        List.rev acc
    | Some line ->
        aux (line :: acc)
  in
  aux []
