let lang1 w=  (List.filter (fun x-> x<>'0' || x<>'1') w )=[] && List.length w >0;;

let rec stringavalida l=match l with
	[]->true
	| hd::tl when hd=0 || hd=1 ->stringavalida tl
	| _->false;;
	
let zero= ref false;;	
let rec lang2 w=match w with
	[]->true
	| '0'::tl-> if !zero=false then (zero:=true;lang2 tl) else false
	| '1'::tl-> if !zero=false then (zero:=true;lang2 tl) else lang2 tl
	| _->false;;
  

let i= ref 1;;
let lang3 w = let length= List.length w in
	let rec controllo w= match w with
		[]-> !i>=2
		| '0'::tl-> (i:=!i+1;controllo tl)
		| '1'::tl-> if !i=1 || !i=length then false else (i:=!i+1;controllo tl)
		| _->false in
	controllo w;;

let uno= ref 0;;
let rec lang4  w=match w with
	[]-> !uno==2
	| '0'::tl-> lang4 tl
	| '1'::tl-> if !uno<2 then (uno:=!uno+1;lang4 tl) else false
	| _->false;;

let lang5 w= let length=List.length w in
	let rec controllo w=match w with
	[]-> length>=2
	|'0'::'0'::tl->controllo tl
	|'1'::'1'::tl ->controllo tl 
	| _-> false in
	controllo w;;
	
	
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
