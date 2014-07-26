{
open LambdamanParser

let table = Hashtbl.create 108

let _ =
  Hashtbl.add table "LDC"  LDC;
  Hashtbl.add table "LD"   LD;
  Hashtbl.add table "ADD"  ADD;
  Hashtbl.add table "SUB"  SUB;
  Hashtbl.add table "MUL"  MUL;
  Hashtbl.add table "DIV"  DIV;
  Hashtbl.add table "CEQ"  CEQ;
  Hashtbl.add table "CGT"  CGT;
  Hashtbl.add table "CGTE" CGTE;
  Hashtbl.add table "ATOM" ATOM;
  Hashtbl.add table "CONS" CONS;
  Hashtbl.add table "CAR"  CAR;
  Hashtbl.add table "CDR"  CDR;
  Hashtbl.add table "SEL"  SEL;
  Hashtbl.add table "JOIN" JOIN;
  Hashtbl.add table "LDF"  LDF;
  Hashtbl.add table "RTN"  RTN;
  Hashtbl.add table "DUM"  DUM;
  Hashtbl.add table "RAP"  RAP;
  Hashtbl.add table "TSEL" TSEL;
  Hashtbl.add table "TAP"  TAP;
  Hashtbl.add table "TRAP" TRAP;
  Hashtbl.add table "ST"   ST;
  Hashtbl.add table "DBUG" DBUG;
  Hashtbl.add table "BRK"  BRK

let token_of_string s =
  let upper = String.uppercase s in
  begin
    try
      Hashtbl.find table upper
    with
    | Not_found -> failwith ("Unknown token:" ^ s)
  end
}

let ws = [' ' '\t' '\n' '\r']
let digit = ['0' - '9']
let alpha = ['a' - 'z']
let biga  = ['A' - 'Z']

rule token = parse
    ws+ { token lexbuf }
  | (alpha | biga)+ { token_of_string (Lexing.lexeme lexbuf) }
  | digit+ { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | _ { failwith ("Unknown Token " ^ Lexing.lexeme lexbuf) }
