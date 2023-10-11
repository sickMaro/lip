(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
         
let rec toklist_of_string w = match w with
	'A'::tl->A::toklist_of_string tl
	| 'B'::tl->B::toklist_of_string tl
	| '='::tl->X::toklist_of_string tl
	| _->[];;

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
let b_pres=ref false;;
let x_pres=ref false;;

let rec valid l=match l with
	[]->true
	| A::tl-> if !b_pres=true || !x_pres=true then false else valid tl
	| X::tl-> if !b_pres=true then false else (x_pres:=true;valid tl)
	| B::tl-> b_pres:=true;valid tl;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)
let rec count_x  x l=match l with
	[]->0
	|hd::tl-> if hd=x then 1+count_x x tl else count_x x tl;;
	
let win l = 
let c_a=count_x A l in
let c_b=count_x B l in
	if c_a>c_b then A else if c_a=c_b then X else B;;
	

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
	A->"winner is A"
	|B->"winner is B"
	|X->"game ended with a tie";;









