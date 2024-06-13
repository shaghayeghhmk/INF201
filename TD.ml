(*2.8.2*)
(*
type jour = int (*{1,...,31}*)
type mois = int (*{1,...,12}*)
type annee = int (*{andeb,...,anfin}*)
type date = jour * mois * annee
type quantieme = int (*1,...,366*)

let andeb:annee = 1900
let anfin:annee = 2100

let mois_en_quant(m:mois): quantieme = 
  match m with
  | 1 -> 0
  | 2 -> 31
  | 3 -> 31+28
  | 4 -> 31+28+31
  | 5 -> 31+28+31+30
  | 6 -> 31+28+31+30+31
  | 7 -> 31+28+31+30+31+30
  | 8 -> 31+28+31+30+31+30+31
  | 9 -> 31+28+31+30+31+30+31+31
  | 10 -> 31+28+31+30+31+30+31+31+30
  | 11 -> 31+28+31+30+31+30+31+31+30+31
  | 12 -> 31+28+31+30+31+30+31+31+30+31+30
  | _ -> 0;;

let est_biss(a:annee):bool=
  a>=andeb && a<= anfin && a mod 4=0 

let result = mois_en_quant(11);;
let date_en_quant((j,m,a) : date):quantieme=
  if (a>=andeb && a<= anfin) && (((m=2 && est_biss(a) && j<=29)||(m=2 && est_biss(a)=false && j<=28)) || ((m=1 ||m=3 ||m=5 ||m=7 ||m=8 ||m=10 ||m=12 )&& j<=31) || ((m=4 ||m=6 ||m=9 ||m=11 ) && j<=30)) then
    if (m>=2 && est_biss(a)) then mois_en_quant(m)+j+1
    else mois_en_quant(m)+j
  else failwith "date_en_quan: invalid date"

let result = date_en_quant(31,12,2012);;

(*2.8.3*)

let mois_en_ch(m:mois):string =
  match m with
  | 1 -> "Janvier"
  | 2 -> "Fevrier"
  | 3 -> "Mars"
  | 4 -> "Avril"
  | 5 -> "Mai"
  | 6 -> "Juin"
  | 7 -> "Juillet"
  | 8 -> "Aout"
  | 9 -> "Septembre"
  | 10 -> "Octobre"
  | 11 -> "Novembre"
  | 12 -> "Decembre"
  | _ -> failwith "mois_en_ch: invalid date";;

let result = mois_en_ch(12);;

let date_en_ch((j,m,a):date):string =
  if j=1 then "1er "^mois_en_ch(m)^" "^string_of_int(a)
  else string_of_int(j)^" "^mois_en_ch(m)^" "^string_of_int(a)
let result = date_en_ch(3,11,2012);;


let dernier_jour(m:mois):jour=
  match m with
  | 1 -> 31
  | 2 -> 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> failwith "dernier_jour: invalid mois";;

let est_fin_mois((j,m,a):date):bool =
  j=dernier_jour(m)

let inc_date_1((j,m,a):date):date=
  if (est_biss(a) && date_en_quant(j,m,a)<366)||(est_biss(a)=false && date_en_quant(j,m,a)<365) then 
    if ((m=2 && est_biss(a) && j=29) || (m=2 && est_biss(a)=false && j=28)) || ((m=1 ||m=3 ||m=5 ||m=7 ||m=8 ||m=10 ||m=12 ) && j=31) || ((m=4 ||m=6 ||m=9 ||m=11) && j=30) then (1,m+1,a)
    else (j+1,m,a)
  else (1,1,a+1)
let inc_date_2((j,m,a):date):date=
  if (est_biss(a) && date_en_quant(j,m,a)<366)||(est_biss(a)=false && date_en_quant(j,m,a)<365) then 
    if ((m=2 && est_biss(a) && j=29) || est_fin_mois(j,m,a)) then (1,m+1,a)
    else (j+1,m,a)
  else (1,1,a+1)

let lendemain_en_ch_1((j,m,a):date):string=
  date_en_ch(inc_date_1(j,m,a))

let lendemain_en_ch_2((j,m,a):date):string=
  date_en_ch(inc_date_2(j,m,a))

let result = lendemain_en_ch_1(28,2,2011);;
let result = lendemain_en_ch_2(28,2,2011);;
*)

(*7.9.1*)
(*Exemple*)
let rec sum = function
  | [] -> 0
  | x :: l -> x + sum(l)
let result0 = sum([3; -2; 5; 3; 1]);;

(* tri insertion *)
let rec insert n l =
  match l with
  |[] -> n::[]
  |h::t -> if h > n then n::l
           else h::(insert n t);;
let result1 = insert 4 ([1; 2; 3; 5; 6]);;

let rec tri_insertion = function
|  [] -> [] 
|  t::q -> insert t (tri_insertion q)
let result2 = tri_insertion([3; -2; 5; 3; 1]);;

(* tri selection *)
let rec find_min lst =
  match lst with
  | [] -> failwith "Empty list"
  | [x] -> x
  | x :: xs -> let min_rest = find_min xs in
               if x < min_rest then x else min_rest;;
let result3 = find_min([3; -2; 5; 3; 1]);;
let rec remove_min x lst= 
  match lst with
  | [] -> []
  | y :: ys ->
    if y = x then ys
    else y :: remove_min x ys;;


let rec tri_selection lst=
  match lst with
  | [] -> []
  | lst ->
    let min = find_min lst in
    let rest = remove_min min lst in
    min :: tri_selection rest;;

let result4 = tri_selection([3; -2; 5; 3; 1]);;

(* tri pivot *)

let rec partition (inf, p ,sup) lst = 
  match lst with
  | [] -> (inf, p ,sup)
  | x :: xs -> if x <= p then partition (x :: inf, p ,sup) xs
              else partition (inf, p ,x :: sup) xs

let rec quicksort lst = 
  match lst with
  |[]-> []
  |p::s -> let (inf,p,sup) = partition([],p,[]) s in 
  (quicksort inf) @ (p :: quicksort sup)
let result5 = quicksort([3; -2; 5; 3; 1]);;


let rec appSuppr (x:'a) (s: 'a list): bool*'a list=
match s with
|[] -> (false, [])
|pr::fin -> if x=pr then true,fin else let (b,fin2)=appSuppr x fin in b,pr::fin2

let result6 = appSuppr 5 ([3; -2; 5; 3; 1]);;

let rec estAna (s1:'a list) (s2: 'a list):bool=
match s1 with 
|[]->s2=[]
|pr::fin->let b,fin2 = appSuppr pr s2 in b && estAna fin fin2

let result7 = estAna['a';'r';'b';'r';'e'] ['b'; 'a'; 'r'; 'r'; 'e'];;

let rec concat (s1:'a list) (s2: 'a list):'a list =
  match s1 with
  | [] -> s2
  | x :: xs -> x :: concat xs s2;;

let rec deb (s: 'a list):'a list=
  match s with
  |[]->[]
  |[pr]->[]
  |pr::fin->pr::deb fin;;

let rec lespref (s: 'a list) :('a list list) =
  match s with
  |[]->[[]]
  |s->lespref(deb(s))@[s];;

lespref([1; 2; 3]);;

let estPtfx (f: 'a ->'a)(x:'a):bool=
f(x)=x;;

let carre (x:int):int = 
  x*x;;

let identite (x:int):int = 
  x;;

let rec estIdempot(f: 'a ->'a)(dom:'a list):bool=
match dom with
|[]->true
|pr::fin-> f(f(pr))=pr && estIdempot f fin;;

let result1 = estIdempot carre [1; 2; 3];;
let result2 = estIdempot identite [1; 2; 3];;

(*g(f(x))*)
let comp (f: 'a -> 'b) (g: 'b -> 'c) : 'a -> 'c =
  fun x -> g (f (x));;

let (>>) (f: 'a -> 'b) (g: 'b -> 'c) : 'a -> 'c =
  fun x -> g (f (x));;

let suiv(x:int):int=
  x+1

let suiv2:(int->int)=
  fun (x) -> x+1

let plus2:(int->int)=
  suiv >> suiv;;

let plus3:(int->int)=
  suiv2 >> suiv2 >> suiv2;;

plus2(4);;
plus3(4);;

let fois2:(int->int)=
  fun x -> x*2;;

let ( * ):(int->int)=
  fun x -> x*2;;
(*Attention on peut pas dire ( * ) sans une space avant et apres etoile*)

let fois2_2 :(int->int) =
  fun x -> ( * ) x;;

fois2_2(4);;

let f1 :(int->int)=
  fois2 >> suiv;;

f1(2);; 

let f2 :(int->int)=
  suiv >> fois2;;

f2(2);;

let f3 :(int->int)=
  fun x -> suiv(fois2(x));;

f3(2);;


let f4 :(int->int)=
  ( * ) >> suiv;;

f4(2);;

let f5 :(int->int)=
  suiv >> ( * );;

f5(2);; 

(*étant donné f, map(e1,e2,e3,...) , (f(e1),f(e2),f(e3),...) *)
let rec map (f:'a->'b)(s: 'a list):'b list=
match s with
|[]->[]
|pr::fin->f(pr)::map f fin;;

(*étant donné C0,init  foldleft(e0,e1,e2,e3,...) , C0(...(C0(C0(C0(init e0)e1)e2)...e(n-1)) *)
let rec foldleft (c0:'b->'a->'b)(init:'b)(s:'a list):'b=
match s with
|[]->init
|pr::fin->foldleft c0 (c0 init pr) fin;;

(*étant donné C0,init  foldright(e0,e1,e2,e3,...) , C0(e0...(C0 (e(n-1) init))) *)
let rec foldright (c0: 'a -> 'b -> 'b) (s: 'a list) (init: 'b): 'b =
  match s with
  | [] -> init
  | pr :: fin -> c0 pr (foldright c0 fin init);;


let f:('a ->'a)=
fun e->e+1;;

let incremente (s:int list):int list=
List.map f s
(*list.map (fun e->e+1) s*)

let incremente2 : int list -> int list=
List.map f 

let incremente3 : int list -> int list=
List.map( (+) 1)

let quorest(n:int (*n!=0*)) : int list->(int*int) list =
List.map (fun e->e/n , e mod n)

let cVs : char -> string=
String.make 1;;

String.make 3 'c';;

let seqcVstring: char list -> string=
List.fold_left(fun acc e -> acc^cVs e) ""

(*acc = accumulateur = valeur initiale*)
(* inja "" valeur initiale*)

let max (a : 'a) (b : 'a) : 'a =
  if a > b then a else b;;

let maxs : int list -> int =
  List.fold_left max 0;;
  
let inverselist : 'a list -> 'a list =
  List.fold_left(fun acc e -> e::acc) [];;

let inverselist2 (s:'a list):'a list=
List.fold_right(fun e acc-> acc@[e] ) s [];;
(*8.4.6*)
let ieme (i:int(*>=1*)) (s:'a list):'a=
List.hd(snd(
  List.fold_right
  (fun e (pos,acc)-> 
  if pos=i then (pos,[List.hd acc]) 
  else(*pos<>i*)(pos+1, List.tl acc)) s (1,s)));;

let applat(s: 'a list list):'a list=
List.fold_right(fun e acc-> e@acc) s [];;

let map1(f:'a->'b)(s:'a list):'b list=
List.fold_right(fun e acc-> (f e)::acc) s [];;

let conj1 : bool list -> bool=
List.fold_left(&&) true;;

let conj2 : bool list -> bool=
List.for_all(fun e->e=true);;

let pour_tout(p:'a -> bool)(s:'a list):bool=
conj1(List.map p s);;

let pour_tout2(p:'a -> bool):('a list->bool)=
List.fold_left(fun acc e-> acc && (p e)) true

let pour_tout3(p:'a -> bool)(s:'a list):bool=
List.fold_left(fun acc e-> acc && (p e)) true s;;

let non(p:'a -> bool):'a->bool=
fun x->not(p x);;

type 'a option = Some of 'a | None 

 let rec find_opt(p:'a -> bool)(s:'a list):('a option)=
 match s with 
 |[]-> None
 |pr::fin->if p pr then Some pr else find_opt p fin;;

let app(x:'a)(s:'a list):bool=
let b = find_opt(fun e -> x=e) s in
match b with
|None -> false
|_-> true;;

let rec filtre (p:'a->bool)(s:'a list):'a list=
match s with
|[]->[]
|pr::fin->(if p pr then [pr] else [])@(filtre p fin);;

let app(x:'a)(s:'a list):bool=
(filtre(fun e->e=x) s)!=[];;
(*__________________________________________________________________________________________________________________________________________*)
(*10*)
(*racine->noeud->feiulle*)
let ab (r:'a): 'a bin = ab(ag,r,ad);;
let abUng (g,r:('a bin*'a)):'a bin;;
let abUnd (r,d:'a*'a bin):'a bin;;

(*profondeur*)
(*5elem->max:5prof,min->3prof*)
(*n elem->max:n prof*)
(*O elem->arbre vide p(0)*)

let rec prof(a:'a abin):int(*>=0*)=
match a with
|Av->0
|ab(g,r,d)->1+max(prof g,prof d);;

let rec appArb(e:'a)(a: 'a abin):bool=
match a with
|Av->false
|ab(g,r,d)->e=r|| appArb e g || appArb e d;;

let rec appArb2(e:'a)(a: 'a abin):bool=
(a != AV) && (e=rac a || appArb2 e (gauche a)||appArb2 e (droite a));;

let rec abVseq(a:'a abin):'a list=
match a with
|Av->[]
|ab(g,r,d)->(abVseq g) @ (r::(abVseq d));;

let appArb3(e:'a)(a: 'a bin):bool=
List.mem e (abVseq a);;

let rec nbf_deniv(n:int(*>0*))(a:'a abin):int(*>=0*)=
match n,a with
|_,Av->0
|1,Ab(g,r,d)->if g=AV && d=AV then 1 else 0
|_,Ab(g,r,d)->nbf_deniv (n-1) g + nbf_deniv (n-1) d;;

let rec niv (e:'a)(a: 'a bin):int(*>=0*)=
match a with
|Av->0
|ab(g,r,d)->if e=r then 1 else (let nivg= niv e g in (if nivg <> 0 then nivg+1) else (let nivd = niv e d in (if nivd <> 0 then nivd+1 else 0))) ;;

(*________________________________________________________________________________________________________________________________________*)
(*8,6,6*)
let rec partition (p:'a -> bool)(s:'a list): 'a list*'a list=
match s with
|[]->[],[]
|pr::fin->let p1,p2=partition p fin in 
if p pr then pr::p1,p2
else p1,pr::p2

(*8,6,7*)
let partition (p:'a -> bool)(s:'a list): 'a list*'a list=
filtre p s,filtre (fun x-> not(p x)) s;;

(*8.7*)
(*tri pivot*)

let s =[3;5;1;0;4;2]
let pivot=3
let s1=[1;0;2]
let s2=[5;4]

type piece = float(*0.01, 0.02, 0.05, 0.10, 0.20, 0.50, 1, 2*)
let les_pieces:piece list;;
let ajoutAchq(x:'a):('a list list)->'a list list=
list.map(fun e->e@[x])

let part 'a list -> 'a list list=
fold_left(fun acc e -> acc@ajoutAchq e acc)[[]]

let somme(s:piece list) : float=
fold_left(fun acc e -> acc +. e)(0.)(s)

let combinaison(piece list): (float * piece list) list=
List.map(fun (acc e) -> somme e*.e )part s