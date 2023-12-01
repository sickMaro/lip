%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token AND
%token NOT
%token OR
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF


%left OR
%left AND
%right NOT
%right ISZERO
%right PRED
%right SUCC

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT e1 = expr; { Not(e1) }
  | e1 = expr; AND e2 = expr; { And(e1,e2) }
  | e1 = expr; OR e2 = expr; { Or(e1,e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | ZERO { Zero }
  | SUCC e1 = expr; { Succ(e1) }
  | PRED e1 = expr; { Pred(e1) }
  | ISZERO e1 = expr; { IsZero(e1) }
  | LPAREN; e=expr; RPAREN {e}
;

