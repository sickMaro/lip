%{
open Rule

let rec get_range n n2 =
  if n > n2 then []
  else n :: get_range (n + 1) n2


%}

%token <string> NUM
%token START
%token MID
%token COMMA
%token DOTS
%token EXSTART
%token SLASH
%token EOF

%start <rule> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
 | EXSTART  START? seq1=num_sequence_ext; SLASH; MID? seq2=num_sequence_ext { Rule (seq1,seq2) }
 | EXSTART  START? seq1=num_sequence_ext; SLASH; MID?  { Rule (seq1,[]) }
 | EXSTART  START? SLASH; MID? seq2=num_sequence_ext; { Rule ([],seq2) }
 | START seq1=num_sequence; SLASH; MID seq2=num_sequence { Rule (seq1,seq2) }
 
num_sequence_ext:
 | n=NUM; COMMA s=num_sequence_ext { int_of_string n::s }
 | n=NUM; DOTS n2=NUM { get_range (int_of_string n)  (int_of_string n2) }
 | n=NUM; DOTS n2=NUM; COMMA s=num_sequence_ext {  get_range (int_of_string n)  (int_of_string n2) @ s  }
 | n=NUM { [int_of_string n] }
 
 num_sequence:
 | n=NUM s=num_sequence { int_of_string n :: s }
 | n=NUM { [int_of_string n] }
 
;
