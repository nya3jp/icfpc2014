%{
open Lambdaman

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

%token LDC
%token LD
%token ADD
%token SUB
%token MUL
%token DIV
%token CEQ
%token CGT
%token CGTE
%token ATOM
%token CONS
%token CAR
%token CDR
%token SEL
%token JOIN
%token LDF
%token AP
%token RTN
%token DUM
%token RAP
%token TSEL
%token TAP
%token TRAP
%token ST
%token DBUG
%token BRK
%token COMMA

%token <int>INTEGER

%token EOF

%type <Lambdaman.instruction> instruction
%start instruction

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

instruction :
    LDC value { LLdc (Int32.of_int $2) }
  | LD value value { LLd ($2, $3) }
  | ADD { LAdd }
  | SUB { LSub }
  | MUL { LMul }
  | DIV { LDiv }
  | CEQ { LCeq }
  | CGT { LCgt }
  | CGTE { LCgte }
  | ATOM { LAtom }
  | CONS { LCons }
  | CAR { LCar }
  | CDR { LCdr }
  | SEL value value { LSel ($2, $3) }
  | JOIN { LJoin }
  | LDF value { LLdf $2 }
  | AP value { LAp $2 }
  | RTN { LRtn }
  | DUM value { LDum $2 }
  | RAP value { LRap $2 }
  | TSEL value value { LTsel ($2, $3) }
  | TAP value { LTap $2 }
  | TRAP value { LTrap $2 }
  | ST value value { LSt ($2, $3) }
  | DBUG { LDbug }
  | BRK { LBrk }

value :
    INTEGER { $1 }

