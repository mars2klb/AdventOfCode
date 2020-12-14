let mask_rgx = Str.regexp "^mask = \\([X01]+\\)"
let mem_rgx = Str.regexp "^mem\\[\\([0-9]+\\)\\] = \\([0-9]+\\)"

let binstring_of_int d =
  if d = 0 then "0"
  else
    let rec aux acc d =
      if d = 0 then acc else
        aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    String.concat "" (aux [] d)

let int_of_binstring d =
  if d = "0" then 0
  else
    let num = String.index d '1'
              |> Str.string_after d
              |> String.to_seq
              |> List.of_seq
    in
    List.fold_left (fun acc x -> (acc lsl 1) + if x = '0' then 0 else 1) 0 num

let dump_stanza stanza =
  let mask, group = stanza in
  mask ^ "\n" ^ String.concat "\n"
                  (List.map (fun (idx, value) ->
                       let binstring = binstring_of_int (int_of_string value) in
                       "  " ^ string_of_int idx ^ ":" ^ value ^ "\n"
                       ^ "   " ^ binstring ^ " <-> " ^ string_of_int (int_of_binstring binstring)) group)

let rec read input stanzas =
  let rec read_group mask lines entries =
    match lines with
    | hd :: tl -> (match Str.first_chars hd 3 with
                   | "mas" -> read lines (stanzas @ [(mask, entries)])
                   | "mem" -> if Str.string_match mem_rgx hd 0
                              then (let idx = Str.matched_group 1 hd in
                                    let value = Str.matched_group 2 hd in
                                    read_group mask tl (entries @ [(int_of_string idx, value)]))
                              else raise (Failure "bad mem match")
                   | _ -> raise (Failure "invalid entry"))
    | [] -> stanzas @ [(mask, entries)]
  in
  let head = List.hd input in
  if Str.string_match mask_rgx head 0
  then
    let mask = Str.matched_group 1 head in
    read_group mask (List.tl input) []
  else
    raise (Failure "bad mask match")

let run () =
  let data = Lib.In_channel.read_lines "day14/test" in
  let input = read data [] in
  String.concat "\n" (List.map dump_stanza input)
