{
open Parser
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white { read lexbuf }
  | newline { read lexbuf } 
  | '#' int { NUMBER }
  | int as d { INT (int_of_string d) }
  | '@' { AT }
  | ',' { COMMA }
  | ':' { COLON }
  | 'x' { X }
  | eof { EOF }
