open Core

type duplicate = Pair of int option * int option

let count_letters str =
  let count_map = Array.create ~len:26 0 in
  let pos c = (Char.to_int c) - 97 in
  let chars = String.to_list str in
  let rec register_letter map = function
    | c::cs -> (
        map.((pos c)) <- map.((pos c)) + 1;
        register_letter map cs
      )
    | [] -> () in
  register_letter count_map chars;
  Pair ((Array.find ~f:(fun x -> x = 2) count_map), (Array.find ~f:(fun x -> x = 3) count_map))
      
let checksum lines =
  let add_counts acc line =
    let (twos, threes) = acc in
    match (count_letters line) with
    | Pair (Some _, Some _) -> (twos+1, threes+1)
    | Pair (Some _, None) -> (twos+1, threes)
    | Pair (None, Some _) -> (twos, threes+1)
    | _ -> (twos, threes) in
  let (twos, threes) = List.fold lines ~init:(0,0) ~f:add_counts in
  twos * threes

let () =
  let input = In_channel.read_lines("input") in
  let answer = checksum input in
  Out_channel.output_string stdout ((Int.to_string answer) ^ "\n")
