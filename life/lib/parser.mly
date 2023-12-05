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
  (* Rule for "S" and "B" sequences *)
  | S seq1=base_rule; SLASH; B; seq2=base_rule; EOF; { Rule(seq1, seq2) }

  (* Rule for "E" sequences (Extended rule) *)
  | E option(S); seq1=separated_list(COMMA, num_with_range); SLASH; option(B); seq2=separated_list(COMMA, num_with_range); EOF;
    { Rule(List.flatten seq1, List.flatten seq2) }
;

base_rule:
  (* Rule for parsing single digit number on not extended rules *)
  | n=NUM { string_to_list n }

num:
  (* Rule for parsing a numeric value *)
  | n=NUM { int_of_string n }
;

range:
  (* Rule for parsing a range of numeric values *)
  | n1=num; DOTS; n2=num { get_range n1 n2 }
;

num_with_range:
  (* Rule for a numeric value or a range *)
  | n=num { [n] }
  | r=range { r }
;


