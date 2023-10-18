open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

let rec count x l =
  match l with
  | [] -> 0
  | hd :: tl -> if hd = x then 1 + count x tl else count x tl

let rec sublist n l =
  match l with
  | [] -> []
  | hd :: tl -> if n > 0 then hd :: sublist (n - 1) tl else []

let rec insert cp l =
  match l with
  | [] -> [cp]
  | hd :: tl -> if snd cp > snd hd then cp :: hd :: tl else  hd::insert cp tl

let rec mem x l=match l with
	[]-> false
	| hd::tl-> if fst hd=x then true else mem x tl;;
	
let create_cp lista =
  let rec crea_cp l listaf =
    match l with
    | [] -> listaf
    | hd :: tl -> if (mem hd listaf)=false then
      let updated_list = insert (hd, count hd lista) listaf in
       crea_cp tl updated_list
       else crea_cp tl listaf 
  in
  crea_cp lista []

let frequency n l =
  sublist n (create_cp l)


