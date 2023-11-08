{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']?

rule read_token =
  parse
  | white { read_token lexbuf } 
  | num { NUM ( Lexing.lexeme lexbuf) } 
  | 'S'{ START }
  | 'E' { EXSTART }
  | 'B' { MID }
  | '/' { SLASH }
  | ',' { COMMA }
  | ".." { DOTS }
  | eof { EOF }

