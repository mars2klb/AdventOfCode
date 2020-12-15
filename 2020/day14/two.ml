(* let table = Hashtbl.create 100 *)

let overlay mask number =
  let one = List.of_seq (String.to_seq mask) in
  let two = List.of_seq (String.to_seq number) in
  String.of_seq (List.to_seq (List.map2 (fun x y -> match x with
                                                    | '1' | 'X' -> x
                                                    | _ -> y) one two))

let handle_stanzas entry =
  let mask, stanzas = entry in
  List.iter (fun (mem, value) ->
      let memmask = overlay mask (Fnord.binstring_of_int mem) in
      print_endline ("MEMMASK: " ^ memmask ^ "  val:" ^ string_of_int value)
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
  "blah"
