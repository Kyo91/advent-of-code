open Core

let parse_line line =
  let number = String.slice line 1 0 in
  match String.get line 0 with
  | '+' -> Int64.of_string number
  | '-' -> Int64.(neg (of_string number))
  | _ -> Int64.of_int 0
  

let rec parse_lines ?(acc=Int64.of_int(0)) = function
  | [] -> acc
  | line::lines ->
    let new_acc = Int64.(acc + (parse_line line)) in
    parse_lines ~acc:new_acc lines
  

let () =
  let input = In_channel.read_lines("input.txt") in
  let answer = parse_lines input in
  Out_channel.output_string stdout ((Int64.to_string answer) ^ "\n")
