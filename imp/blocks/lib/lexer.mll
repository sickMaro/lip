{
open Parser
}

let white = [' ' '\t']+
let letters = ['a'-'z' 'A'-'Z']
let ch = ['a'-'z' 'A'-'Z' '0'-'9']
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let id = letters ch*
rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "or" { OR }
  | "and" { AND }
  | "not" { NOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | "int" { INT }
  | "bool" { BOOL }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | eof { EOF }
