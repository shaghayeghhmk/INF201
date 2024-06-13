(*Le profil, la sémantique et les exemples de chaque question sont dans le compte rendu (fichier Word).*)
(*Question1*)
type candidat = string
type bulletin = candidat
type urne = bulletin list
type panel = candidat list
type score = int

(*Question 2*)
let rec compte (c:candidat)(u:urne):score=
  match u with 
  |[]-> 0 
  |x::suite-> if x=c then 1+(compte c suite) else (compte c suite);;  

(*Test_Question 2*)
let lc1 = ["Eric";"Kyle";"Stan"];;
let u1 = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";
"Eric";"Eric";"Stan";"Stan"];;
compte "Eric" u1;;

(*Question 3*)
type resultat = (candidat * score) list

let rec depouiller (lc:panel)(u:urne):resultat=
  match lc with  
  |[]-> []
  |x::suite -> (x,(compte x u))::(depouiller suite u);;

(*Test_Question 3*)
depouiller lc1 u1;;

(*Question 4*)
let rec union (r1: resultat) (r2: resultat) : resultat =
  match r1, r2 with
  | [], [] -> []
  | (x1, s1) :: suite1, (x2, s2) :: suite2 -> (x1, s1 + s2) :: union suite1 suite2
  | _, _ -> failwith "longueurs de listes sont différentes";;

(*Test_Question 4*)
let r1 = [("Eric", 5); ("Kyle", 1); ("Stan", 5)];;
let r2 = [("Eric", 4);("Kyle", 2); ("Stan", 6)];;

let r3= union r1 r2 ;;

(*Question 5*)

let rec max_depouille (l: resultat): candidat*score =
  match l with
  | [] -> failwith "La liste ne peut pas être vide"
  | [x] -> x
  | (a1, b1) :: (a2, b2) :: suite ->
      let (candidat, score) = max_depouille ((if b1 > b2 then (a1, b1) else (a2, b2)) :: suite) in
      if b1 > score then (a1, b1) else (candidat, score);;

(*Test_Question 5*)
max_depouille r3;;

(*Question 6*)
let vainqueur_scrutin_uninominal (u: urne) (lc: panel) : candidat =
  let resultats = depouiller lc u in
  let (candidat_vainqueur, _) = max_depouille resultats in
  candidat_vainqueur;;

(*Test_Question 6*)
vainqueur_scrutin_uninominal u1 lc1;;

(*Question 7*)
let rec suppr_elem (l:'a list)(e:'a):'a list=
  match l with 
  |[]-> []
  |x::suite->if x=e then suppr_elem suite e
      else x::suppr_elem suite e;; 

let deux_premiers (u:urne)(p:panel):(candidat*score)*(candidat*score)= 
  let liste1=depouiller p u in
  let premier=max_depouille liste1 in
  let candidat_premier=vainqueur_scrutin_uninominal u p in
  let nouvelle_urne=suppr_elem u candidat_premier in 
  let liste2=depouiller p nouvelle_urne in 
  let second=max_depouille liste2 in
  premier, second;;

(*Reponse de question 8,9,10 dans le compte rendu(fichier word)*)

(*Question 11*)
type mention = Arejeter | Insuffisant | Passable | Assezbien | Bien  | Tresbien;;
type bulletin_jm = mention list;;
type urne_jm = bulletin_jm list;;

(*Question 12*)
let u3 : urne_jm = [[Tresbien; Assezbien; Arejeter; Passable]; (* Premier bulletin *)
[Assezbien; Assezbien; Arejeter; Tresbien]; (* Second bulletin *)
[Tresbien; Arejeter; Arejeter; Tresbien]] (* Troisi`eme bulletin *)

let rec depouille_jm (u:urne_jm) =
  match u with
  | [] -> []
  | []::_ -> []
  | u -> (List.map List.hd u) :: depouille_jm (List.map List.tl u);;

(*Test_Question 12*)
let ms = depouille_jm u3;;

let u=[[Tresbien; Assezbien; Arejeter; Passable]; (* Premier bulletin *)
[Assezbien; Tresbien; Passable; Tresbien]; (* Second bulletin *)
[Passable; Arejeter; Arejeter; Insuffisant]] (* Troisi`eme bulletin *);;

let ms = depouille_jm u;;

(*Question 13*)
let tri (l:'a list):'a list = List.sort compare l;;

let tri_mention (mll: mention list list):mention list list=
  List.map tri mll ;; 

(*Test_Question 13*)
tri [Assezbien; Bien; Arejeter; Tresbien];;
let ms_triee = tri_mention ms;;

(*Question 14*)
let mediane (liste:bulletin_jm):mention =
  let n = List.length liste in 
  List.nth liste (n / 2);;

(*Test_Question 14*)
tri [Assezbien; Bien; Arejeter; Tresbien];;
List.map mediane ms_triee;;

(*Question 15*)
let meilleure_mediane (mll:mention list list):mention =
  let n=List.length mll in 
  let liste=tri(List.map mediane (tri_mention mll)) in
  List.nth liste (n-1) ;;

(*Test_Question 15*)
meilleure_mediane ms_triee;;

(*Question 16*)
let supprime_perdants (mll: mention list list): mention list list =
  let meilleur_mediane = meilleure_mediane mll in 
  List.map (fun bulletin -> 
    if mediane bulletin >= meilleur_mediane then 
      bulletin
    else
      []) mll;;

(*Test_Question 16*)
supprime_perdants ms_triee;;

(*Question 17*)
let rec supprime_mention (m:mention) (l:bulletin_jm):bulletin_jm =
  match l with
  | [] -> []
  | hd::tl -> if hd = m then tl else hd :: supprime_mention m tl
;;
let supprime_meilleure_mediane (l:urne_jm):urne_jm =
  let non_empty_lists = List.filter (fun x -> x <> []) l in
  let best_median = meilleure_mediane non_empty_lists in
  List.map (fun lst -> if lst = [] then [] else supprime_mention best_median lst) l
;;

(*Test_Question 17*)
supprime_mention Arejeter [Arejeter; Bien; Arejeter] ;;
supprime_meilleure_mediane ms_triee;;

(*Question 18*)
let vainqueur_jm (mll:mention list list):string =
  let candidats = List.init (List.length mll) (fun i -> i) in
  let filtered_candidates = List.filter (fun i -> (List.nth mll i) != []) candidats in
  match filtered_candidates with
  | [i] -> string_of_int i
  | _ ->
    let medians = List.map (fun i -> mediane (List.nth mll i)) filtered_candidates in
    let max_median = List.fold_left max Arejeter medians in
    let max_count = List.fold_left (fun acc m -> if m = max_median then acc + 1 else acc) 0 medians in
    if max_count = 1
    then string_of_int (List.find (fun i -> mediane (List.nth mll i) = max_median) filtered_candidates)
    else ""
;;

(*Test_Question 18*)
vainqueur_jm [[];[Passable]; []; []];;
vainqueur_jm [[Passable; Tresbien; Tresbien]; [Arejeter; Bien; Tresbien];
   [Arejeter; Arejeter; Passable]; [Insuffisant; Passable; Tresbien]];;

(*Reponse de question 20 dans le compte rendu(fichier word)*)

(*Question 21*)
type ville = string * resultat;;
type zone = Reg of string | Dpt of string;;
type arbre = Bv of ville | N of zone * arbre list;;

(*Question 22*)
let rec trouve_bv (a: arbre) (nom_bv: string) : resultat =
  match a with
  | Bv (v, res) -> if v = nom_bv then res else []
  | N (_, people) ->
    let results = List.map (fun pip -> trouve_bv pip nom_bv) people in
    try
      List.find (fun r -> r <> []) results
    with Not_found -> []
;;

(*Test_Question 22*)
let ara =
N (Reg "Auvergne-Rh^one-Alpes",
[N (Dpt "Dr^ome",
[Bv ("Valence",
[("ARTHAUD", 161); ("ROUSSEL", 595); ("MACRON", 7756); ("LASSALLE", 590);
("LE PEN", 4679); ("ZEMMOUR", 2080); ("M´ ELENCHON", 8398);
("HIDALGO", 519); ("JADOT", 1701); ("P´ ECRESSE", 1423); ("POUTOU", 186);
("DUPONT-AIGNAN", 573)]);
Bv ("Romans-sur-Is`ere",
[("ARTHAUD", 181); ("ROUSSEL", 371); ("MACRON", 4030); ("LASSALLE", 334);
("LE PEN", 3270); ("ZEMMOUR", 1072); ("M´ ELENCHON", 4108);
("HIDALGO", 251); ("JADOT", 850); ("P´ ECRESSE", 631); ("POUTOU", 111);
("DUPONT-AIGNAN", 341)])]);
N (Dpt "Is`ere",
[Bv ("Meylan",
[("ARTHAUD", 28); ("ROUSSEL", 169); ("MACRON", 4457); ("LASSALLE", 164);
("LE PEN", 1288); ("ZEMMOUR", 928); ("M´ ELENCHON", 2198);
("HIDALGO", 251); ("JADOT", 906); ("P´ ECRESSE", 763); ("POUTOU", 64);
("DUPONT-AIGNAN", 162)]);
Bv ("Echirolles",
[("ARTHAUD", 104); ("ROUSSEL", 506); ("MACRON", 3276); ("LASSALLE", 259);
("LE PEN", 2737); ("ZEMMOUR", 779); ("M´ ELENCHON", 5121);
("HIDALGO", 223); ("JADOT", 590); ("P´ ECRESSE", 360); ("POUTOU", 92);
("DUPONT-AIGNAN", 202)]);
Bv ("Fontaine",
[("ARTHAUD", 55); ("ROUSSEL", 363); ("MACRON", 2111); ("LASSALLE", 146);
("LE PEN", 1835); ("ZEMMOUR", 541); ("M´ ELENCHON", 3113);
("HIDALGO", 185); ("JADOT", 493); ("P´ ECRESSE", 212); ("POUTOU", 83);
("DUPONT-AIGNAN", 121)]);
Bv ("Saint-Martin-d'H`eres",
[("ARTHAUD", 58); ("ROUSSEL", 436); ("MACRON", 2769); ("LASSALLE", 207);
("LE PEN", 2289); ("ZEMMOUR", 661); ("M´ ELENCHON", 4763);
("HIDALGO", 242); ("JADOT", 777); ("P´ ECRESSE", 300); ("POUTOU", 119);
("DUPONT-AIGNAN", 161)]);
Bv ("Gières",
[("ARTHAUD", 16); ("ROUSSEL", 66); ("MACRON", 1071); ("LASSALLE", 84);
("LE PEN", 641); ("ZEMMOUR", 205); ("M´ ELENCHON", 844); ("HIDALGO", 96);
("JADOT", 301); ("P´ ECRESSE", 155); ("POUTOU", 30);
("DUPONT-AIGNAN", 61)]);
Bv ("Grenoble",
[("ARTHAUD", 256); ("ROUSSEL", 1300); ("MACRON", 15968);
("LASSALLE", 845); ("LE PEN", 6444); ("ZEMMOUR", 3389);
("M´ ELENCHON", 24568); ("HIDALGO", 1488); ("JADOT", 5644);
("P´ ECRESSE", 2019); ("POUTOU", 508); ("DUPONT-AIGNAN", 661)])])])

(* Exemple d'utilisation *)
let res_gieres = trouve_bv ara "Gières";;

(*Question 23*)
let resultat_grenoble = trouve_bv ara "Grenoble";;
let resultat_fontaine = trouve_bv ara "Fontaine";;
let resultat_valence = trouve_bv ara "Valence";;

let resultat_total = union (union resultat_grenoble resultat_fontaine) resultat_valence;;



