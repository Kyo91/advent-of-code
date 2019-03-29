%token <int> INT
%token NUMBER
%token COMMA
%token AT
%token COLON
%token X
%token EOF

%start <[`Box of int * int * int * int] list> boxes

%%

boxes:
  | EOF { [] }
  | b = box bs = boxes
    { b :: bs }
;

box:
  | NUMBER; AT; xmin = INT; COMMA; ymin = INT; COLON; width = INT; X; height = INT
    { `Box (xmin, ymin, width, height) }
;

%%
