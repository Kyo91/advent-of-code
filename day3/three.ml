open Core

let () =
  let claims = In_channel.with_file "input" ~f:Claim.get_all in
  List.iter claims ~f:(fun claim -> Out_channel.output_string stdout (Claim.pp claim))
