{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = ['0'-'9' 'A'-'F' 'a'-'f']+
let num2 = "0x" hex | num
rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "*" { MOL }
  | "0x" hex { CONST(Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | "-" num2 { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
