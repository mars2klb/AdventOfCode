type command = { direction : char; steps : int }
type point = { x : int; y : int }

let split node =
  let opcode = node.[0] in
  let value = String.split_on_char opcode node
              |> List.tl
              |> List.hd
              |> int_of_string
  in
  {direction=opcode; steps=value}

let rec unseen node list =
  match list with
  | [] -> true
  | hd :: rest -> if hd = node
                  then false
                  else unseen node rest

let intersections lines =
  let first = List.hd lines in
  let second = List.tl lines
               |> List.hd in
  let crosses = ref [] in
  let rec check node nodes =
    match nodes with
    | [] -> !crosses
    | hd :: rest -> if not (unseen hd first) && not (hd = {x=0; y=0})
                    then crosses := hd :: !crosses; check hd rest
  in
  check {x=0; y=0} second

let walk origin node =
  Printf.printf "dir: %c  steps: %d  " node.direction node.steps;
  let path = ref [] in
  let x = origin.x in
  let y = origin.y in
  match node with
  | {direction='U'; steps} -> Printf.printf "U%d\n" steps;
                              for i = 0 to steps do
                                if unseen {x=x; y=(y + i)} !path
                                then path := {x=x; y=(y + i)} :: !path
                              done; !path
  | {direction='D'; steps} -> Printf.printf "D%d\n" steps;
                              for i = 0 to steps do
                                if unseen {x=x; y=(y - i)} !path
                                then path := {x=x; y=(y - i)} :: !path
                              done; !path
  | {direction='L'; steps} -> Printf.printf "L%d\n" steps;
                              for i = 0 to steps do
                                if unseen {x=(x - i); y=y} !path
                                then path:= {x=(x - i); y=y} :: !path
                              done; !path
  | {direction='R'; steps} -> Printf.printf "R%d\n" steps;
                              for i = 0 to steps do
                                if unseen {x=(x + i); y=y} !path
                                then path := {x=(x + i); y=y} :: !path;
                              done; !path
  | _ -> !path

let manhattan_distance point =
  abs point.x + abs point.y

let closest smallest lst =
    List.fold_left (fun acc x -> min acc x) smallest lst

let draw line =
  print_endline "DRAW";
  let path = ref [] in
  let rec work origin node nodes =
    match nodes with
    | [] -> !path
    | hd :: rest -> path := (walk origin (split hd)) @ !path;
                    work (List.hd !path) hd rest
  in
  work {x=0; y=0} "" (String.split_on_char ',' line)

let () =
  let lines = In_channel.read_lines "test" in
  let dimensions = ref [] in
  let rec measure lines =
    match lines with
    | [] -> ()
    | hd :: rest -> dimensions := (draw hd) :: !dimensions; measure rest
  in
  measure lines;
  let ints = intersections !dimensions in
  List.iter (fun p -> Printf.printf " => %d, %d: %d\n" p.x p.y (manhattan_distance p)) ints;
  Printf.printf "closest: %d\n" (closest 10000 (List.map manhattan_distance ints))
