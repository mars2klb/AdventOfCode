let filter lines by =
  let results = ref [] in
  List.iteri (fun idx line -> if idx mod by = 0
                              then results := !results @ [line]) lines;
  !results

let run () =
  let data = Lib.In_channel.read_lines "day3/input" in
  let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
  let trees = List.map (fun (x, y) ->
                  let filtered = filter data y in
                  One.compute filtered 0 0 x) slopes in
  string_of_int (List.fold_left ( * ) 1 trees) ^ " trees"
