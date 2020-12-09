open Lib

let parse opcode =
  if Str.string_match (Str.regexp "\\([a-z]+\\) \\([+|-][0-9]+\\)") opcode 0
  then
    let code = Str.matched_group 1 opcode in
    let delta = int_of_string (Str.matched_group 2 opcode) in
    (code, delta)
  else
    raise (Failure "parsing failure")

let rec compute opcodes cmd trail =
  let offset, acc = cmd in
  try
    let _ = List.find (fun x -> x = offset) trail in acc
  with Not_found -> ();
  let code, delta = parse (List.nth opcodes offset) in
  match code with
  | "nop" -> compute opcodes (offset + 1, acc) (trail @ [offset])
  | "acc" -> compute opcodes (offset + 1, acc + delta) (trail @ [offset])
  | "jmp" -> compute opcodes (offset + delta, acc) (trail @ [offset])
  | _ -> raise (Failure "bad opcode")

let run () =
  let data = In_channel.read_lines "day8/input" in
  "last good acc: " ^ string_of_int (compute data (0, 0) [])
