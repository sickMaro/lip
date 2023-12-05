{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf } 
  | num { NUM ( Lexing.lexeme lexbuf) } 
  | 'S'{ S }
  | 'E' { E }
  | 'B' { B }
  | '/' { SLASH }
  | ',' { COMMA }
  | ".." { DOTS }
  | eof { EOF }

