open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int option

let string_of_result = function
  | Some(n) -> string_of_int n
  | None -> "Divisione per 0"


let app_op op x y=match (x,y) with
	(Some x, Some y)-> Some( op x y)
	| _ -> None 
	
	
let rec eval = function
  | Const n -> Some n
  | Add (e1, e2) -> app_op (+) (eval e1) (eval e2)
  | Minus (e1, e2) -> app_op (-) (eval e1) (eval e2)
  | Mol (e1, e2) -> app_op ( * ) (eval e1) (eval e2)
  | Div (e1, e2) -> if eval e2=Some 0 then None else app_op (/) (eval e1) (eval e2)
  | Um (e1) ->  match  eval e1 with
  		Some x-> Some (-x)
  		| _ -> None
  
                    
