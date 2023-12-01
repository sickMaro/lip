open Ast

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not(e1) -> "Not(" ^ (string_of_expr e1) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e1) -> "Succ(" ^ (string_of_expr e1) ^ ")"
  | Pred(e1) -> "Pred(" ^ (string_of_expr e1) ^ ")"
  | IsZero(e1) -> "IsZero(" ^ (string_of_expr e1) ^ ")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
let rec trace1 = function
    Not(True) -> False
  | Not(False) -> True
  | Not(e1) -> let e1' = trace1 e1 in Not(e1')
  
  | And(True,e2) -> e2
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  
  | Or(True,_) -> True
  | Or(False,e2) -> e2
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  
  | Succ(e1) -> let e1' = trace1 e1 in Succ(e1')
  | Pred(Succ(e1)) -> e1
  | Pred(e1) -> let e1' = trace1 e1 in Pred(e1')
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e1) -> let e1' = trace1 e1 in IsZero(e1')
  
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

type exprtype = BoolT | NatT
exception TypeError of string;;

let string_of_type t = match t with
    BoolT -> "Bool"
    | NatT -> "Nat"

let rec typecheck = function
  | True | False -> BoolT

  | Not e ->
      let expr_type = typecheck e in
      if expr_type = BoolT then BoolT
      else raise (TypeError (string_of_expr e ^ " has type " ^ string_of_type expr_type ^ ", but type Bool was expected"))

  | And (e1, e2) | Or (e1, e2) ->
      let expr_type1 = typecheck e1 in
      let expr_type2 = typecheck e2 in
      if expr_type1 <> BoolT then raise (TypeError (string_of_expr e1 ^ " has type " ^ string_of_type expr_type1 ^ ", but type Bool was expected"))
      else if expr_type2 <> BoolT then raise (TypeError (string_of_expr e2 ^ " has type " ^ string_of_type expr_type2 ^ ", but type Bool was expected"))
      else BoolT

  | If (e0, e1, e2) ->
      let expr_type0 = typecheck e0 in
      if expr_type0 <> BoolT then raise (TypeError (string_of_expr e0 ^ " has type " ^ string_of_type expr_type0 ^ ", but type Bool was expected"))
      else
        let expr_type1 = typecheck e1 in
        let expr_type2 = typecheck e2 in
        if expr_type2 <> expr_type1 then raise (TypeError (string_of_expr e2 ^ " has type " ^ string_of_type expr_type2 ^ ", but type " ^ string_of_type expr_type1 ^ " was expected"))
        else expr_type1

  | Zero -> NatT

  | Succ e | Pred e ->
      let expr_type = typecheck e in
      if expr_type = NatT then NatT
      else raise (TypeError (string_of_expr e ^ " has type " ^ string_of_type expr_type ^ ", but type Nat was expected"))

  | IsZero e ->
      let expr_type = typecheck e in
      if expr_type = NatT then BoolT
      else raise (TypeError (string_of_expr e ^ " has type " ^ string_of_type expr_type ^ ", but type Bool was expected"))



type exprval = Bool of bool | Nat of int

let string_of_val e = match e with
    Bool b -> string_of_bool b
  | Nat n -> string_of_int n

  
let rec eval = function
  | True -> Bool true
  | False -> Bool false
  | Not e1 -> Bool (not (match eval e1 with Bool b -> b | _ -> failwith "L'espressione non è di tipo booleano"))
  | And (e1, e2) -> Bool (match (eval e1, eval e2) with (Bool a, Bool b) -> a && b | _ -> failwith "Gli operandi non sono booleani")
  | Or (e1, e2) -> Bool (match (eval e1, eval e2) with (Bool a, Bool b) -> a || b | _ -> failwith "Gli operandi non sono booleani")
  | If (e0, e1, e2) -> if (match eval e0 with Bool b -> b | _ -> failwith "La condizione non è booleana") then eval e1 else eval e2
  | Zero -> Nat 0
  | Succ e1 -> (match eval e1 with Nat b -> Nat (b + 1) | _ -> failwith "L'operando non è un numero")
  | Pred e1 -> (match eval e1 with Nat b -> if b > 0 then Nat (b - 1) else failwith "Predecessore negativo" | _ -> failwith "L'operando non è un numero")
  | IsZero e1 -> Bool (match eval e1 with Nat b -> b = 0 | _ -> failwith "L'operando non è un numero")
;;
