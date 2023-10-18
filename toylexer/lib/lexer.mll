{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let capital=['A'-'Z']
let vowels=['a' 'e' 'i' 'o' 'u']
let vowels2=vowels[ 'A' 'E' 'I' 'O' 'U']
let atok=capital chr*
let btok=vowels*
let consonant=['^' 'a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U' '0'-'9'] 
let ctok=consonant* vowels2? consonant*
let dtok=['-']?['0'-'9']*['.']?['0'-'9']*
let etok="0x"|"0X" chr+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
  | atok { ATOK }
  | btok { BTOK} 
  | ctok {CTOK }
  | dtok {CTOK }
  | etok {ETOK }
