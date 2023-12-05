%{
(* Open the Rule module *)

open Rule

(* Function to generate a range of integers *)
let rec get_range n n2 =
  if n > n2 then []
  else n :: get_range (n + 1) n2

(* Function to convert a string to a list of integers, only for not extended rules*)
let string_to_list str =
  List.map (fun c -> int_of_string (String.make 1 c)) (List.of_seq (String.to_seq str))
%}

%token <string> NUM
%token S B COMMA DOTS E SLASH EOF

%start <rule> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  (* Base rules *)
  | S; seq1=NUM; SLASH; B; seq2=NUM; EOF; { Rule(string_to_list seq1,string_to_list seq2) }

  (* Extended rule *)
  | E; option(S); seq1=separated_list(COMMA,extended_rule); SLASH; option(B); seq2=separated_list(COMMA,extended_rule); EOF;
    { Rule(List.flatten seq1, List.flatten seq2) }
;

extended_rule:
 (* Rule for parsing numbers or range on extended rules *)
 | n=NUM; { [int_of_string n] }
 | n1=NUM; DOTS; n2=NUM; { get_range (int_of_string n1)  (int_of_string n2) }
;


