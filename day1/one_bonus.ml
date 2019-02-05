open Core

module IntSet = Set.Make(Int)

let parse_line line =
  let number = String.slice line 1 0 in
  match String.get line 0 with
  | '+' -> Int.of_string number
  | '-' -> Int.(neg (of_string number))
  | _ -> Int.of_int 0
  

let rec parse_lines memory acc = function
  | [] -> Error (acc, memory)
  | line::lines ->
    let new_acc = Int.(acc + (parse_line line)) in
    match (IntSet.mem memory new_acc) with
    | true -> Ok new_acc
    | false -> parse_lines (IntSet.add memory new_acc) new_acc lines
  
let rec repeat_frequency input memory acc =
  match parse_lines memory acc input with
  | Error (acc, memory) -> Printf.printf "Current accum: %d\n" acc;
    repeat_frequency input memory acc
  | Ok acc -> acc
  

let () =
  let input = In_channel.read_lines("input.txt") in
  let memory = IntSet.empty in
  let answer = repeat_frequency input memory 0 in
  Out_channel.output_string stdout ((Int.to_string answer) ^ "\n")
