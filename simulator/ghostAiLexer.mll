{
open GhostAiParser

let table = Hashtbl.create 108
let _ =
  Hashtbl.add table "MOV" MOV;
  Hashtbl.add table "INC" INC;
  Hashtbl.add table "DEC" DEC;
  Hashtbl.add table "ADD" ADD;
  Hashtbl.add table "SUB" SUB;
  Hashtbl.add table "MUL" MUL;
  Hashtbl.add table "DIV" DIV;
  Hashtbl.add table "AND" AND;
  Hashtbl.add table "OR"  OR;
  Hashtbl.add table "XOR" XOR;
  Hashtbl.add table "JLT" JLT;
  Hashtbl.add table "JEQ" JEQ;
  Hashtbl.add table "JGT" JGT;
  Hashtbl.add table "INT" INT;
  Hashtbl.add table "HLT" HLT;
  Hashtbl.add table "PC"  PC
;;

let token_of_string s =
  let upper = String.uppercase s in
  try
    Hashtbl.find table upper
  with
    Not_found -> failwith ("Unknown token: " ^ s)
}

let ws = [' ' '\t' '\n' '\r']
let digit = ['0' - '9']
let alpha = ['a' - 'z']
let biga  = ['A' - 'Z']

rule token = parse
    ws+ { token lexbuf }
  | (alpha|biga)  { REGISTER (String.uppercase (Lexing.lexeme lexbuf)) }
  | (alpha|biga)+ { token_of_string (Lexing.lexeme lexbuf) }
  | "["   { LPAREN }
  | "]"   { RPAREN }
  | ","   { COMMA }
  | digit+ { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | _ { failwith ("Unknown Token " ^ Lexing.lexeme lexbuf) }
