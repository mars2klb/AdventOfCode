let table = Hashtbl.create 100

let rec churn inputs =
  match inputs with
  | hd :: tl -> let mask, stanzas = hd in
                List.iter (fun (idx, x) ->
                    Hashtbl.replace table idx (Fnord.overlay mask (Fnord.binstring_of_int x))) stanzas;
                churn tl
  | [] -> ()

let run () =
  let data = Lib.In_channel.read_lines "day14/input" in
  let input = Fnord.read data [] in
  churn input;
  let acc = ref 0 in
  Hashtbl.iter (fun _ v -> acc := !acc + Fnord.int_of_binstring v) table;
  string_of_int !acc
