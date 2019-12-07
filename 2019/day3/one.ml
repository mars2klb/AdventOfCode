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

let walk origin node =
  Printf.printf "-origin- %d,%d\n" origin.x origin.y;
  let path = ref [] in
  let x = origin.x in
  let y = origin.y in
  match node with
  | {direction='U'; steps} -> for i = 0 to steps do
                                if unseen {x=x; y=(y + i)} !path
                                then path := {x=x; y=(y + i)} :: !path; Printf.printf " U%d: %d, %d\n" steps x (y + i)
                              done; !path
  | {direction='D'; steps} -> for i = 0 to steps do
                                if unseen {x=x; y=(y - i)} !path
                                then path := {x=x; y=(y - i)} :: !path; Printf.printf " D%d: %d, %d\n" steps x (y - i)
                              done; !path
  | {direction='L'; steps} -> for i = 0 to steps do
                                if unseen {x=(x - i); y=y} !path
                                then path:= {x=(x - i); y=y} :: !path; Printf.printf " L%d: %d, %d\n" steps (x - i) y
                              done; !path
  | {direction='R'; steps} -> for i = 0 to steps do
                                if unseen {x=(x + i); y=y} !path
                                then path := {x=(x + i); y=y} :: !path; Printf.printf " R%d: %d, %d\n" steps (x + i) y
                              done; !path
  | _ -> !path

let draw line =
  let path = ref [] in
  let rec work origin node nodes =
    match nodes with
    | [] -> !path
    | hd :: rest -> path := (walk origin (split hd)) @ !path; work (List.hd !path) hd rest
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
  let print_line line =
    Printf.printf "origin: %d,%d\n" (List.hd line).x (List.hd line).y;
    List.iter (fun p -> Printf.printf "  %d, %d\n" p.x p.y) line
    (* Printf.printf "%d, %d\n" line.x line.y *)
  in
  (* print_line ["no"] *)
  List.iter print_line !dimensions
  (* for i = 0 to List.length dimensions do
   *   let line = dimensions.(i) in
   *   Printf.printf "list length: %d\n" (List.length line)
   *   (\* for j = 0 to List.length dimensions.[i] do
   *    *   Printf.printf "-> %d,%d\n" dimensions.[i].[j]
   *    * done *\)
   * done
   * (\* List.map (Printf.printf "Result: %d\n") dimensions.[0] *\) *)
