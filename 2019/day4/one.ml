(* --- Day 4: Secure Container ---
 * 
 * You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.
 * 
 * However, they do remember a few key facts about the password:
 * 
 * It is a six-digit number.
 * The value is within the range given in your puzzle input.
 * Two adjacent digits are the same (like 22 in 122345).
 * Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
 * Other than the range rule, the following are true:
 * 
 * 111111 meets these criteria (double 11, never decreases).
 * 223450 does not meet these criteria (decreasing pair of digits 50).
 * 123789 does not meet these criteria (no double).
 * How many different passwords within the range given in your puzzle input meet these criteria?
 * 
 * Your puzzle input is 273025-767253. *)
(* https://gitlab.com/unduthegun/adventofcode2019/blob/master/04/secure_container.ml *)

let input = (273025, 767253)

let range from until = List.init (until - from + 1) (fun x -> x + from)

let filter_increasing password =
  let rec loop prev = function
    | 6 -> true
    | i ->
       let curr = password.(i) in
       if curr < prev
       then false
       else loop curr (i + 1)
  in
  loop ~-1 0

let filter_repeats password =
  let rec loop prev = function
    | 6 -> false
    | i ->
       let curr = password.(i) in
       if curr = prev
       then true
       else loop curr (i + 1)
  in
  loop ~-1 0

let () =
  let inputs =
    match input with from, until -> range from until in
  let candidates =
    inputs
    |> List.to_seq
    |> Seq.map (fun entry ->
           entry
           |> string_of_int
           |> String.to_seq
           |> Seq.map int_of_char
           |> Array.of_seq)
  in
  let filtered =
    candidates |> Seq.filter (fun x -> filter_increasing x && filter_repeats x)
  in
  Printf.printf "Part 1: %u\n" (filtered |> List.of_seq |> List.length)
