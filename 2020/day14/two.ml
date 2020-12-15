let table = Hashtbl.create 100

let overlay mask number =
  let one = List.of_seq (String.to_seq mask) in
  let two = List.of_seq (String.to_seq number) in
  String.of_seq (List.to_seq (List.map2 (fun x y -> match x with
                                                    | '1' | 'X' -> x
                                                    | _ -> y) one two))

let count_x mask =
  let m = List.of_seq (String.to_seq mask) in
  2.0 ** float_of_int (List.fold_left (fun acc x -> acc + if x = 'X' then 1 else 0) 0 m)

let handle_stanzas entry =
  let mask, stanzas = entry in
  List.iter (fun (mem, value) ->
      let memmask = overlay mask (Fnord.binstring_of_int mem) in
      print_endline ("MEMMASK: " ^ memmask ^ "  val:" ^ string_of_int value ^ " count:" ^ string_of_float (count_x memmask));
      Hashtbl.replace table memmask value
    ) stanzas

let rec churn inputs =
  match inputs with
  | hd :: tl -> handle_stanzas hd;
                churn tl
  | [] -> ()

let run () =
  let data = Lib.In_channel.read_lines "day14/test2" in
  let inputs = Fnord.read data [] in
  churn inputs;
  let acc = ref 0 in
  Hashtbl.iter (fun mask value ->
      let variations = count_x mask in
      acc := !acc + (int_of_float variations) * value) table;
  string_of_int !acc
