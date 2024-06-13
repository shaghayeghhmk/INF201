(* Trois plats principaux possibles : *) 
type platPrincipal = P1 | P2 | P3 

(* Une seule entrée possible, mais en trois tailles : *) 
type tailleEntree = Petite | Moyenne | Grande 

(* x morceaux de fromage à l’unité : M(x), ou au plateau : P *) 
type fromage = M of int | P 

(* Trois desserts possibles : *)
type dessert = D1 | D2 | D3 

type  cout = int(*entier positif*)

let coutEntree(t:tailleEntree): cout =
match t with
  | Petite -> 3
  | Moyenne -> 5
  | Grande -> 7
let cstPRIXPRINCIPAL:cout = 10 
let cstSUPLP3:cout = 3

(*profil: coutPrincipal : platPrincipal -> cout*)
(*sémantique: coutPrincipal(p) renvoie le coût d’un plat principal.*)
(*Exemple:
  coutPrincipal (P1)=10
  coutPrincipal (P2)=10
  coutPrincipal (P3)=13*)

let coutPrincipal (p: platPrincipal) : cout = 
  match p with
  | P1 -> cstPRIXPRINCIPAL
  | P2 -> cstPRIXPRINCIPAL
  | P3 -> cstPRIXPRINCIPAL + cstSUPLP3

let cstPRIXFROMAGEUNITAIRE: cout = 2
let cstPRIXPLATEAU: cout = 6

let coutFromage (f: fromage) : cout =
  match f with
  | M(x) -> x * cstPRIXFROMAGEUNITAIRE
  | P -> cstPRIXPLATEAU
(*test*)
let result = coutFromage(M(2));;

coutFromage (P) < coutFromage (M(4))

let coutDessert (_:dessert) : cout = 
  5


(*test*)
let result = coutDessert(D2);;

type fromOUdess= D of dessert | F of fromage
(*la nature de 𝐷 et 𝐹 sont dessert et fromage*)
(*Le profil de D est {dessert} -> {fromOUdess} et le profil de F est {fromage} -> {fromOUdess}.*)
type repas = 
| F1 of tailleEntree * platPrincipal 
| F2 of platPrincipal * fromOUdess
let formule1:repas = F1(Petite,P3)
let formule2:repas = F2(P1,F(M(1)))

let coutRepas(r:repas):cout=
  match r with
  | F1(t, p) -> coutEntree(t) + coutPrincipal(p)
  | F2(p, D(d)) -> coutPrincipal(p) + coutDessert(d)
  | F2(p, F(f)) -> coutPrincipal(p) + coutFromage(f)

let result= coutRepas(formule1);;
let result=coutRepas(formule2);;


type fromETOUdess=
  |D of dessert
  |F of fromage
  |FromETdess of fromage*dessert

type repas2 = 
| F1 of tailleEntree * platPrincipal 
| F2 of platPrincipal * fromOUdess
| F3 of tailleEntree * fromETOUdess

let coutRepas2(r:repas2):cout=
  match r with
  | F1(t, p) -> coutEntree(t) + coutPrincipal(p)
  | F2(p, D(d)) -> coutPrincipal(p) + coutDessert(d)
  | F2(p, F(f)) -> coutPrincipal(p) + coutFromage(f)
  | F3(t, D(d)) -> coutEntree(t) + coutDessert(d)
  | F3(t, F(f)) -> coutEntree(t) + coutFromage(f)
  | F3(t, FromETdess(f,d)) -> coutEntree(t) + coutFromage(f) + coutDessert(d)

let result= coutRepas2(F3(Petite,FromETdess(M(1),D3)));;
let result=coutRepas2(F3(Petite,F(M(1))));;
(*_____________________________________________________________________________________________________________________*)

type intensite = int (* restreint à {0, ..., 255} *) 

type tripletRVB = intensite*intensite*intensite

let rouge : tripletRVB = (255,0,0)
let vert : tripletRVB = (0,255,0)
let bleu : tripletRVB = (0,0,255)
let noir : tripletRVB = (255,255,255)
let blanc : tripletRVB = (0,0,0)

(*Spécification:
Profil:     estGris : tripletRVB -> bool
sémentique: estGris(r,b,v) retourne true si les trois composantes ont la même intensité c_à_d r=v=b
Exemples:   estGris(128, 128, 128) = true  estGris (255, 0, 0) = false 
*)
let estGris ((r,b,v):tripletRVB):bool= 
  r=b && b=v

(*Spécification:
Profil:     niveauNoir : tripletRVB -> float
sémentique: niveauNoir(r,b,v) retourne le pourcentage de noir présent dans ce triplet gris
Exemples:   niveauNoir(128, 128, 128)=100  niveauNoir(128, 128, 128) = 50.1960784313725483  niveauNoir(0, 0, 0) = 0 
*)
let niveauNoir ((r,b,v):tripletRVB):float= 
  if estGris(r,v,b) then (float_of_int(r)*.100.)/.255.
  else failwith "le coulour n'est pas gris"

(*test*)
let result=niveauNoir(128, 128, 128);;

let barbouille(t1:tripletRVB)(t2:tripletRVB):tripletRVB =
  let (r1,v1,b1)=t1 and (r2,v2,b2)=t2 in
  if r1+r2<=255 && v1+v2<=255 && b1+b2<=255 then (r1+r2,v1+v2,b1+b2)
  else if r1+r2>255 then (255,v1+v2,b1+b2)
  else if v1+v2>255 then (r1+r2,255,b1+b2)
  else (r1+r2,v1+v2,255)

(*test*)
let jaune:tripletRVB=(255,255,0)
let result=barbouille(rouge)(vert);;
let result=barbouille(jaune)(bleu);;

(*a) Dans la définition 𝑐𝑜𝑢𝑙𝑒𝑢𝑟 def, 𝑅𝑜𝑢𝑔𝑒, 𝑉𝑒𝑟𝑡 et 𝐵𝑙𝑒𝑢 sont des couleurs primaires de base du modèle RGB (rouge, vert et bleu respectivement). 
𝑁𝑜𝑖𝑟(𝑝) est une couleur de niveau de gris, où 𝑝 représente l'intensité de la couleur grise (0 étant noir et 100 étant blanc). 
𝑅𝑣𝑏(𝑡) est une couleur définie à partir d'un triplet RVB, où chaque composante est un entier compris entre 0 et 255.*)

type p = float(*restrint à {0,...,100}*)
type t = tripletRVB
type couleur = Rouge|Bleu|Vert|Noir of p|Rvb of t

let estPrimaire(t:tripletRVB):bool=
  t=rouge || t=bleu || t=vert

let cyan:tripletRVB=(0,255,255)
let megenta:tripletRVB=(255,0,255)

let complementaire(couleur):couleur =
  match couleur with
  | Rouge -> Rvb (0, 255, 255)
  | Vert -> Rvb (255, 0, 255)
  | Bleu -> Rvb (255, 255, 0)
  | Noir(p) -> Noir (100. -. p)
  | Rvb (r, v, b) -> Rvb (255 - r, 255 - v, 255 - b)

let couleurVtriplet(c:couleur):tripletRVB=
  match c with
  | Rouge -> rouge
  | Vert -> vert
  | Bleu -> bleu
  | Noir(p) -> let x=(int_of_float((p*.255.)/.100.)) in (x,x,x)
  | Rvb (r, v, b) -> (r,v,b)

let barbouilleC(c1:couleur)(c2:couleur):couleur=
  Rvb(barbouille(couleurVtriplet(c1))(couleurVtriplet(c2)))
(*test*)
let result=barbouilleC(Rvb(56,4,23))(Bleu);;
(*_________________________________________________________________________________________________________________*)
(*EXO1*)
let rec factoriel (n: int):int = n*factoriel(n-1)
let result= factoriel(5);;

let rec factorielle n =
  if n <= 1 then 1
  else n * factorielle (n - 1)

let result= factorielle(5);;
(*EXO2*)

type natP = Z | S of natP 
let rec double(n:natP):natP=
  match n  with
  |Z -> Z
  |S(n) -> S(S(double(n)))

let rec triple(n:natP):natP=
  match n  with
  |Z -> Z
  |S(n) -> S(S(S(triple(n))))


let result= double(S(Z));;


let rec plusk(k:int)(n:natP):natP=
  if k=0 then n
  else S(plusk(k-1)(n))
let result= plusk(4)(S(S(Z)));;
let rec kuple(k:int)(n:natP):natP=
  match n with
  |Z -> Z
  |S(n) -> plusk(k)(kuple(k)(n))

let result= kuple(4)(S(S(Z)));;
  
let triple2(n:natP):natP=
  kuple(3)(n)
let result= triple(S(Z));;
let result= triple2(S(Z));;


(*EXO3*)
type enseigne = Pique|Trefle|Coeur|Carreau
(*type énuméré*)
type petite =int(*restraint à{7,8,9,10}*)*enseigne
(*type produit*)
type tete=Valet|Dame|Roi|As
(*type énuméré*)
type honneur=tete*enseigne
(*type produit*)
type couleur=Rouge|Noir
(*type énuméré*)

type carte= P of petite|H of honneur|J of couleur
(*type somme*)

let jokerRouge:carte= J(Rouge)

(*Profil:
  rang: carte -> {1,...,9}
  Sémentique: rang(c)est la place de 𝑐 dans l’ordre décrit ci-dessus
  exemple:
  rang(P(8,_))= 2
  rang(J())= 9
  rang(P(10, Pique)) = 4 
  rang(H(As, Coeur)) = 8
*)

let rang (c:carte) : int (* restreint à 1 ,…, 9 *) = 
match c with 
| P(p,_) -> p - 6 
| H(t,_) -> (match t with 
| Valet -> 5 
| Dame -> 6 
| Roi -> 7 
| As -> 8)
| J(_) -> 9

let coul(c:carte):couleur=
match c with 
| P(_,e) | H(_,e)-> (match e with 
| Carreau | Coeur -> Rouge
| Trefle | Pique-> Noir)
| J(cl) -> cl

type resultat = Gagne | Perdu | Bataille

let joueur(c1:carte)(c2:carte):resultat=
if rang(c1)>rang(c2) then Gagne
else if rang(c1)<rang(c2) then Perdu
else
  if coul(c1)=coul(c2) then Bataille
  else if coul(c1)=Rouge && coul(c2)=Noir then Gagne
  else Perdu

(*Exo4*)
type n = int (* ⩾ 0 *) 
type longueur = Toise of n | Pied of n | Pouce of n

let troisTs : longueur = Toise(3) 
let douzePc : longueur = Pouce(12)

type toise = n
type pied = n
type pouce = n
type longueurD = toise * pied * pouce

let troisTsD : longueurD = (3,0,0)
and douzePcD : longueurD = (0,0,12)
let longVlongD(l:longueur):longueurD=
match l with
|Toise(n)->(n,0,0)
|Pied(n)->(0,n,0)
|Pouce(n)->(0,0,n)
(*Profil:
  longVlongD: longueur->longueurD *)
let result=longVlongD(troisTs)

let longDVlong((x,y,z):longueurD):longueur=
if x>0 then Toise(x)
else if y>0 then Pied(y)
else Pouce(z)


let result=longDVlong(troisTsD)
let result=longDVlong(0,6,0)
let result=longDVlong(0,0,0)

let reprCan((x,y,z):longueurD):longueurD=
let total_x = x+ (z/12 + y)/6 in
let reste_y = (y + z/12) mod 6 in 
let rest_z = z mod 12 in
(total_x,reste_y,rest_z)

let result=reprCan(5,16,24)

let longDVstr((x,y,z):longueurD):string=
let iVs=string_of_int and (t,pd,pc)=reprCan(x,y,z) in
let toise = if t>0 then iVs(t)^" toise " else "" in
let pied = if pd>0 then iVs(pd)^" pied " else "" in
let pouce = if pc>0 then iVs(pc)^" pouce" else "" in
  toise^pied^pouce

(*test*)
let result=longDVstr(5,16,24)

(*__________________________________________________________________________________________________________________*)

(*CC Kimia*)
(*type vect2 = float*float
type mat22 = vect2*vect2
type mat2x = V of vect2|M of mat22
let add_vect2((x1,y1):vect2)((x2,y2):vect2):vect2=(x1+.x2,y1+.y2)
let add_mat2x(x:mat2x)(y:mat2x):mat2x=
match (x,y) with
|(V(v1),V(v2))->V(add_vect2(v1)(v2))
|M(v1,v2),M(w1,w2)->M((add_vect2(v1)(w1)),(add_vect2(v2)(w2)))
|_ -> failwith("on peut pas adittioner")
let result=add_mat2x(M((1.0, 2.0),(3.0, 4.0)))(M((3.0, 2.0), (6.0, 4.0)))*)

type vect2 = float*float
type mat22 = float*float*float*float
type mat2x = V of vect2|M of mat22
let  mat22_x_mat2x((a,b,c,d):mat22)(m:mat2x):mat2x=
match m with
|V(x,y) -> V((a*.x+.b*.y),(c*.x+.d*.y))
|M(x,y,z,w) -> M((a*.x+.b*.z),(a*.y+.b*.w),(c*.x+.d*.z),(c*.y+.d*.w))

let result=mat22_x_mat2x(((1.0, 2.0, 3.0, 4.0)))(M((3.0, 2.0, 6.0, 4.0)))
type hexa4 =int(*restreint à{0,...,16^4}*)
type carhex =char(*restreint à{"0",...,"9"}et{"A",...,"F"}*)
type base16 =int(*restreint à{0,...,15}*)

type rep_hexa4 = carhex*carhex*carhex*carhex
type chiffre=char(*restreint à{"0",...,"9"}*)
type base10=int(*restreint à{0,...,9}*)
let chiffreVbase10(c:chiffre):base10=
match c with
| '0' -> 0
| '1' -> 1
| '2' -> 2
| '3' -> 3
| '4' -> 4
| '5' -> 5
| '6' -> 6
| '7' -> 7
| '8' -> 8
| '9' -> 9
| _ -> failwith("Error")

let base16Vhex(e:base16):carhex=
if e<=9 then char_of_int(int_of_char('0')+e)
else char_of_int(int_of_char('A') + e-10 )
let result=base16Vhex(5)
