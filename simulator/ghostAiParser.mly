%{
open Ghost

let int_of_register = function
  | "A" | "a" -> 0
  | "B" | "b" -> 1
  | "C" | "c" -> 2
  | "D" | "d" -> 3
  | "E" | "e" -> 4
  | "F" | "f" -> 5
  | "G" | "g" -> 6
  | "H" | "h" -> 7
  | _ -> failwith "Unknown register"
;;

%}

%token MOV
%token INC
%token DEC
%token ADD
%token SUB
%token MUL
%token DIV
%token AND
%token OR
%token XOR
%token JLT
%token JEQ
%token JGT
%token INT
%token HLT
%token PC
%token LPAREN
%token RPAREN
%token COMMA

%token <string>REGISTER
%token <int>INTEGER

%token EOF

%type <Ghost.ginstruction> instruction
%start instruction

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

instruction :
    MOV value COMMA value { GMov ($2, $4) }
  | INC value { GInc $2 }
  | DEC value { GDec $2 }
  | ADD value COMMA value { GAdd ($2, $4) }
  | SUB value COMMA value { GSub ($2, $4) }
  | MUL value COMMA value { GMul ($2, $4) }
  | DIV value COMMA value { GDiv ($2, $4) }
  | AND value COMMA value { GAnd ($2, $4) }
  | OR value COMMA value  { GOr ($2, $4) }
  | XOR value COMMA value { GXor ($2, $4) }
  | JLT INTEGER COMMA value COMMA value { GJlt ($2, $4, $6) }
  | JEQ INTEGER COMMA value COMMA value { GJeq ($2, $4, $6) }
  | JGT INTEGER COMMA value COMMA value { GJgt ($2, $4, $6) }
  | INT INTEGER { GInt $2 }
  | HLT { GHlt }

value :
    INTEGER { GVConst $1 }
  | REGISTER { GVReg (int_of_register $1) }
  | PC { GVPCReg }
  | LPAREN INTEGER RPAREN { GVIndConst $2 }
  | LPAREN REGISTER RPAREN { GVIndReg (int_of_register $2) }

