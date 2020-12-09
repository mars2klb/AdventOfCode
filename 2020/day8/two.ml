let rec compute opcodes cmd trail =
  let offset, acc = cmd in
  (try
    let _ = List.find (fun x -> x = offset) trail in
    raise (Failure "infinite loop")
  with Not_found -> ());
  if offset = List.length opcodes then acc
  else
    let code, delta = One.parse (List.nth opcodes offset) in
     match code with
     | "nop" -> compute opcodes (offset + 1, acc) (trail @ [offset])
     | "acc" -> compute opcodes (offset + 1, acc + delta) (trail @ [offset])
     | "jmp" -> compute opcodes (offset + delta, acc) (trail @ [offset])
     | _ -> acc

let flip opcode =
  let code, delta = One.parse opcode in
  match code with
  | "nop" -> "jmp " ^ string_of_int delta
  | "jmp" -> "nop " ^ string_of_int delta
  | _ -> opcode

let rec churn opcodes offset =
  let iteration = List.mapi (fun idx code -> if idx = offset then flip code else code) opcodes in
  try
    compute iteration (0, 0) []
  with Failure _ ->
    churn opcodes (offset + 1)

let () =
  let data = Lib.In_channel.read_lines "input" in
  print_endline ("Part 2: last good acc: " ^ string_of_int (churn data 0))
