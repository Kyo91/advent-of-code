open Core

let one_off line1 line2 =
  let chars1 = String.to_list line1 in
  let chars2 = String.to_list line2 in
  let rec check_one_off l1 l2 one_off =
    match (l1, l2) with
    | (b::bs, c::cs) -> (
        let diff = not (c = b) in
        match diff, one_off with
        | true, true -> false
        | true, false -> check_one_off bs cs true
        | false, _ -> check_one_off bs cs one_off
      )
    | [], [] -> one_off
    | _ -> false  in
  check_one_off chars1 chars2 false

let find_common line1 line2 =
  let common_chars bs cs =
    List.fold2_exn bs cs ~init:[] ~f:(fun chars b c -> if b = c then b::chars else chars) in
  let reversed = common_chars (String.to_list line1) (String.to_list line2) in
  String.of_char_list (List.rev reversed)

let () =
  let input = In_channel.read_lines("input") in
  let remaining = List.cartesian_product input input |>
              List.filter ~f:(function
                      | (l1, l2) -> one_off l1 l2) in
  let answer = match remaining with
  | (l1,l2)::_ -> Some (find_common l1 l2)
  | _ -> None in
  match answer with
  | Some ans -> Out_channel.output_string stdout (ans ^ "\n")
  | None -> Out_channel.output_string stdout "Nothing Found\n"
