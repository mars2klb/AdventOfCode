let heading = ref 'E'
let cardinals = "ESWN"

let dump dir =
  let direction, distance = dir in
  "[" ^ Char.escaped direction ^ ":" ^ string_of_int distance ^ "]"

let dump_coord coord =
  let x, y = coord in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let distance coord =
  let x, y = coord in
  (abs x, abs y)

let rec navigate move_fn marks from =
  match marks with
  | mark :: [] -> distance (move_fn mark from)
  | mark :: rest -> navigate move_fn rest (move_fn mark from)
  | [] -> raise (Failure "run aground")

let rotate direction distance =
  let current_index = String.index cardinals !heading in
  match direction with
  | 'R' -> let offset = ((distance / 90) + current_index) mod 4 in
           heading := String.get cardinals offset
  | 'L' -> let offset = (((360 - distance) / 90) + current_index) mod 4 in
           heading := String.get cardinals offset
  | _ -> raise (Failure "bad rotation")

let parse dir =
  (String.get dir 0, int_of_string (Str.string_after dir 1))

let rec chart dirs =
  match dirs with
  | dir :: [] -> [parse dir]
  | dir :: rest -> [parse dir] @ (chart rest)
  | [] -> raise (Failure "bork")
