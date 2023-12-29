open Ast

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
let rec trace1 = function

  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;


let rec eval_expr = function
  | True -> Bool true
  | False -> Bool false
  | Const e -> Int e
  | Var e -> 
  | Not e1 -> (match eval e1 with Bool b -> Bool(not b) | _ -> failwith "L'espressione non è di tipo booleano")
  | And (e1, e2) -> Bool (match (eval e1, eval e2) with (Bool a, Bool b) -> a && b | _ -> failwith "Gli operandi non sono booleani")
  | Or (e1, e2) -> Bool (match (eval e1, eval e2) with (Bool a, Bool b) -> a || b | _ -> failwith "Gli operandi non sono booleani")
  | Add (e1, e2) -> Int (match (eval e1, eval e2) with (Int a, Int b) -> a + b | _ -> failwith "Gli operandi non sono interi")
  | Sub (e1, e2) -> Int (match (eval e1, eval e2) with (Int a, Int b) -> a - b | _ -> failwith "Gli operandi non sono interi")
  | Mul (e1, e2) -> Int (match (eval e1, eval e2) with (Int a, Int b) -> a * b | _ -> failwith "Gli operandi non sono interi")
  | Eq (e1, e2) -> Bool (match (eval e1, eval e2) with (Int a, Int b) -> a = b | _ -> failwith "Gli operandi non sono interi")
  | Seq (e1, e2) -> Bool (match (eval e1, eval e2) with (Int a, Int b) -> a <= b | _ -> failwith "Gli operandi non sono interi")
;;
