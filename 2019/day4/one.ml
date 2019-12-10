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
let test_inputs =
  [ 111111; 223440; 123789; 112233; 111122; 112225; 123444; 123455; 111115 ]

let range from until = List.init (until - from + 1) (fun x -> x + from)

let () =
  let inputs =
    match input with from, until -> range from until in
  let possible_passwords =
    inputs
    |> List.to_seq
    |> Seq.map (fun possible ->
           possible
           |> string_of_int
           |> String.to_seq
           |> Seq.map int_of_char
           |> Array.of_seq)
