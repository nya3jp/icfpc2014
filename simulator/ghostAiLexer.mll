{
open GhostAiParser
}

let ws = [' ' '\t' '\n' '\r']
let digit = ['0' - '9']
let alpha = ['a' - 'z']
let biga  = ['A' - 'Z']

rule token = parse
    ws+ { token lexbuf }
  | "MOV" { MOV }
  | "INC" { INC }
  | "DEC" { DEC }
  | "ADD" { ADD }
  | "SUB" { SUB }
  | "MUL" { MUL }
  | "DIV" { DIV }
  | "AND" { AND }
  | "OR"  { OR }
  | "XOR" { XOR }
  | "JLT" { JLT }
  | "JEQ" { JEQ }
  | "JGT" { JGT }
  | "INT" { INT }
  | "HLT" { HLT }
  | "PC"  { PC }
  | "["   { LPAREN }
  | "]"   { RPAREN }
  | ","   { COMMA }
  | biga  { REGISTER (Lexing.lexeme lexbuf) }
  | digit+ { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | _ { failwith ("Unknown Token " ^ Lexing.lexeme lexbuf) }
