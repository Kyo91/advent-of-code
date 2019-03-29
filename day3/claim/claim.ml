open Core

type t =
  { xmin : int;
    xmax : int;
    ymin : int;
    ymax : int;
  }

let from_box = function
  | `Box (xmin, width, ymin, height) ->
    { xmin = xmin;
      xmax = xmin + width;
      ymin = ymin;
      ymax = ymin + height
    }
    
let pp = function
  | {xmin; xmax; ymin; ymax} -> Printf.sprintf "(%d,%d)x(%d,%d)\n" xmin ymin xmax ymax


let get_all file =
  let lexbuf = Lexing.from_channel file in
  Parser.boxes Lexer.read lexbuf |>
  List.map ~f:from_box


