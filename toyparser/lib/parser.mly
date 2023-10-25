%{
open Ast

%}


%token <string> CONST
%token PLUS
%token MINUS
%token MOL
%token DIV
%token UM

%token LPAREN
%token RPAREN
%token EOF


%left PLUS
%left MINUS
%left MOL DIV

%right UM

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Minus(e1,e2) }
  | e1 = expr; MOL; e2 = expr { Mol(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | UM; e2 = expr { Um(e2) }
  | LPAREN; e=expr; RPAREN {e}
;
