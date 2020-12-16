let table = Hashtbl.create 100

let datasets = [(436, [0; 3; 6]);
                (1, [1; 3; 2]);
                (10, [2; 1; 3]);
                (27, [1; 2; 3]);
                (78, [2; 3; 1]);
                (438, [3; 2; 1]);
                (1836, [3; 1; 2]);
                (-1, [19; 20; 14; 0; 9; 1])]

let ponder said =
  try
    let p1, p2 = Hashtbl.find table said in
    if p1 = 0 then 0 else p2 - p1
  with Not_found -> 0

let rec said number turn =
  (* off by one *shrug* *)
  if turn = 2021 then number
  else
    let to_say = ponder number in

    try
      let _, p2 = Hashtbl.find table to_say in
      Hashtbl.replace table to_say (p2, turn);
      said to_say (turn + 1)
    with Not_found -> (Hashtbl.add table to_say (0, turn); said to_say (turn + 1))

let run () =
  let expected, numbers = List.nth datasets 7 in
  List.iteri (fun idx x -> Hashtbl.add table x (0, idx + 1)) numbers;
  "expected:" ^ string_of_int expected ^ " got:" ^ string_of_int (said (List.hd (List.rev numbers)) ((List.length numbers) + 1))
