%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token AND
%token NOT
%token OR
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token PLUS
%token MINUS
%token MUL
%token SKIP
%token ASSIGN
%token SEQ
%token EQ
%token LEQ
%token INT
%token BOOL
%token <string> CONST
%token <string> ID
%token EOF


%left SEQ
%nonassoc ELSE DO
%nonassoc EQ LEQ
%left OR
%left AND
%right NOT
%left PLUS MINUS
%left MUL

%start <cmd> prog

%%

prog:
  | e = cmd; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | e = ID { Var(e) }
  | n = CONST { Const(int_of_string n) }
  | NOT e1 = expr; { Not(e1) }
  | e1 = expr; AND e2 = expr; { And(e1,e2) }
  | e1 = expr; OR e2 = expr; { Or(e1,e2) }
  | e1 = expr; PLUS e2 = expr; { Add(e1,e2) }
  | e1 = expr; MINUS e2 = expr; { Sub(e1,e2) }
  | e1 = expr; MUL e2 = expr; { Mul(e1,e2) }
  | e1 = expr; EQ e2 = expr; { Eq(e1,e2) }
  | e1 = expr; LEQ e2 = expr; { Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN { e }
;


decl:
  | INT; x = ID; SEQ; d = decl; { Intvar(x,d) }
  | BOOL; x = ID; SEQ; d = decl; { Boolvar(x,d) }
  | { EmptyDecl }
;

cmd:
  | SKIP; { Skip }
  | x = ID; ASSIGN; e = expr; { Assign(x,e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e,c1,c2) }
  | WHILE e = expr; DO; c = cmd; { While(e,c) }
  | LBRACE d = decl; c = cmd; RBRACE; { Decl(d,c) }
;

