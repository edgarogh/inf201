(*q1/q2*)
(* Specification: assertion inverse *)
let assert_not cond = assert (not cond);;

(*Types*)
type 'a multielement = 'a * int;;
type 'a multiensemble = 'a multielement list;;

(*Variables pour test*)
let cst_me1 : char multielement = ('a', 3);;
let cst_me2 : char multielement = ('b', 2);;
let cst_me3 : char multielement = ('c', 1);;

let cst_me4 : char multielement = ('a', 1);;
let cst_me5 : char multielement = ('b', 1);;

let cst_me6 : char multielement = ('a', 1);;
let cst_me7 : char multielement = ('b', 3);;
let cst_me8 : char multielement = ('c', 2);;

let cst_mens1 : char multiensemble = [cst_me1; cst_me2; cst_me3];;
let cst_mens2 : char multiensemble = [cst_me4; cst_me5; cst_me3];;
let cst_mens3 : char multiensemble = [cst_me6; cst_me7; cst_me8];;


(*q4*)
type valeur = int;;
type couleur = Rouge | Bleu | Jaune | Noir;; 
type tuile = Joker | T of valeur * couleur;;

(*q5*)
type combinaison = tuile list;;
type table = combinaison list;;
type pose = combinaison list;;

(*q6*)
type 'a multielement = 'a * int;;
type 'a multiensemble = 'a multielement list;; 
type main = tuile multiensemble;;
type pioche = main;;

let cst_PIOCHE_INIT : pioche = 
  [ (Joker, 2) ;
    T(1,Rouge), 2 ; T(2,Rouge), 2 ; T(3,Rouge), 2 ; T(4,Rouge), 2 ; T(5,Rouge), 2 ;
    T(6,Rouge), 2 ; T(7,Rouge), 2 ; T(8,Rouge), 2 ; T(9,Rouge), 2 ; T(10,Rouge), 2 ;
    T(11,Rouge), 2 ; T(12,Rouge), 2 ; T(13,Rouge), 2 ;
    T(1,Bleu), 2 ; T(2,Bleu), 2 ; T(3,Bleu), 2 ; T(4,Bleu), 2 ; T(5,Bleu), 2 ;
    T(6,Bleu), 2 ; T(7,Bleu), 2 ; T(8,Bleu), 2 ; T(9,Bleu), 2 ; T(10,Bleu), 2 ;
    T(11,Bleu), 2 ; T(12,Bleu), 2 ; T(13,Bleu), 2 ;
    T(1,Jaune), 2 ; T(2,Jaune), 2 ; T(3,Jaune), 2 ; T(4,Jaune), 2 ; T(5,Jaune), 2 ;
    T(6,Jaune), 2 ; T(7,Jaune), 2 ; T(8,Jaune), 2 ; T(9,Jaune), 2 ; T(10,Jaune), 2 ;
    T(11,Jaune), 2 ; T(12,Jaune), 2 ; T(13,Jaune), 2 ;
    T(1,Noir), 2 ; T(2,Noir), 2 ; T(3,Noir), 2 ; T(4,Noir), 2 ; T(5,Noir), 2 ;
    T(6,Noir), 2 ; T(7,Noir), 2 ; T(8,Noir), 2 ; T(9,Noir), 2 ; T(10,Noir), 2 ;
    T(11,Noir), 2 ; T(12,Noir), 2 ; T(13,Noir), 2
  ];;

(*q7*)
let trouver_rouge (mens:tuile multiensemble)=
  List.filter
    (fun (el,oc) -> 
       match el with
         | T(_,Rouge) -> true
         |_ -> false)
    mens
;; 

let trouver_jaune (mens:tuile multiensemble)=
  List.filter (fun (el,oc) -> 
                match el with
                  | T(_,Jaune) -> true
                  |_ -> false)
    mens
;;

let trouver_bleu (mens:tuile multiensemble)=
  List.filter (fun (el,oc) -> 
                match el with
                  | T(_,Bleu) -> true
                  |_ -> false)
    mens
;;

let trouver_noir (mens:tuile multiensemble)=
  List.filter (fun (el,oc) -> 
                match el with
                  | T(_,Noir) -> true
                  |_ -> false)
    mens
;;

let trouver_joker (mens:tuile multiensemble)=
  List.filter (fun (el,oc) -> el = Joker) mens
;;

(*tests*)
assert(trouver_rouge [ (Joker, 2); T(1,Rouge), 2; T(2,Bleu), 2 ] = [T(1,Rouge), 2]) ;;
assert(trouver_jaune [ (Joker, 2); T(1,Jaune), 2; T(2,Bleu), 2 ] = [T(1,Jaune), 2]) ;;
assert(trouver_bleu  [ (Joker, 2); T(1,Noir), 2; T(2,Bleu), 2 ]  = [T(2,Bleu), 2]) ;;
assert(trouver_noir  [ (Joker, 2); T(1,Noir), 2; T(2,Bleu), 2 ]  = [T(1,Noir), 2]) ;;
assert(trouver_joker [ (Joker, 2); T(1,Noir), 2; T(2,Bleu), 2 ]  = [(Joker, 2)]);;


let rec insere ((tuile,oc)) (mens:tuile multiensemble) =
  match tuile with
    | T(n, cons) ->(
        match mens with
          | [] -> (T(n,cons),oc)::[]
          | (Joker,_) :: [] -> failwith "impossible trier joker"
          | (Joker,_) :: _ -> failwith "impossible trier joker"
          | (T(n_c, cons_c),oc_n)::queue ->
              if n < n_c then (T(n,cons),oc) :: mens
              else (T(n_c, cons_c),oc_n) :: insere (T(n,cons),oc) queue
      )
    | _ -> failwith "Je gère pas les jokers"
;;


let rec tri_insertion = function
  |  [] -> []
  |  tete::queue -> insere tete (tri_insertion queue)
;;

assert (tri_insertion [T(1,Noir), 3; T(1,Noir), 2; T(1,Noir), 1] 
        = [T(1,Noir), 1; T(1,Noir), 2; T(1,Noir), 3]);;

let en_ordre (mens:tuile multiensemble)= 
  tri_insertion (trouver_rouge mens) @
  tri_insertion (trouver_jaune mens) @
  tri_insertion (trouver_bleu mens)  @
  tri_insertion (trouver_noir mens)  @
  trouver_joker mens
;;

assert (en_ordre
          [T(2,Noir), 3; T(1,Noir), 2; T(2,Bleu), 1;T(1,Rouge), 2; 
           T(2,Rouge), 1;  Joker, 2; T(2,Jaune), 1; T(1,Jaune), 2; 
           T(1,Bleu), 2;] = 
        [(T (1, Rouge), 2); (T (2, Rouge), 1);(T (1, Jaune), 2); (T (2, Jaune), 1);
         (T (1, Bleu), 2); (T (2, Bleu), 1); (T (1, Noir), 2); (T (2, Noir), 3);(Joker, 2)]);;

type joueur = J1 | J2;;
type statut = joueur * bool * main;;
type etat = (statut * statut) * table * pioche * joueur;;

(*q8*)
(*fonctions intermédiaires*)
let rec cardinal (mens: 'a multiensemble) : int =
  match mens with
    | [] -> 0
    | (_, occurences) :: rest -> occurences + cardinal rest
;;

(*Tests*)
assert (0 = cardinal []);;
assert (3 = cardinal cst_mens2);;
assert (6 = cardinal cst_mens1);;

let rec ieme_mel (n: int) (mens: 'a multiensemble) : 'a multielement =
  if n < 0 then
    failwith "n < 0"
  else
    match mens with
      | [] -> failwith "n >= cardinal mens"
      | (value, occurences) :: rest -> (
          if n < occurences then
            (value, 1)
          else
            ieme_mel (n - occurences) rest)
;;

(*Tests*)
assert (('a', 1) = ieme_mel 1 cst_mens1);;
assert (('b', 1) = ieme_mel 4 cst_mens1);;
assert (('c', 1) = ieme_mel 5 cst_mens1);;

let un_dans_mel (mens: 'a multiensemble) : 'a multielement =
  let length = cardinal mens in
    ieme_mel (Random.int length) mens
;;


let rec extraire (n:int)(pioche:'a multiensemble):('a multiensemble*'a multiensemble)= 
  match n with
    | 1 -> let aleatoire:'a multielement = un_dans_mel pioche in
        let mens:'a multiensemble = [aleatoire] in
          (mens,pioche)
    | _ -> 
        let (main_n,pioche_n) = extraire (n-1) pioche in 
        let aleatoire = un_dans_mel pioche in
        let main = en_ordre(aleatoire::main_n)in 
          (main,pioche)
;;

let test = extraire 4 cst_PIOCHE_INIT;;

let distrib () =
  let (main1, pioche_inter) = extraire 14 cst_PIOCHE_INIT in
  let (main2, pioche) = extraire 14 pioche_inter in
    (main1, main2, pioche)
;;

let test = distrib();;

let init_partie ():etat =
  let (main1, main2, pioche) = distrib () in
  let statut1:statut = (J1, false, main1) in
  let statut2:statut = (J2, false, main2) in
  let statut=(statut1,statut2) in
    (statut, [], pioche, J1)
;;

(*q9*)
(*constante de test*)
let statut_1:statut = (J1,false,[(T (2, Rouge), 1);(T (1, Jaune), 2); (T (2, Jaune), 1)]);;
let statut_2:statut = (J2,false,[(T (2, Bleu), 1);(T (1, Bleu), 2); (T (2, Bleu), 1)]) ;;
let table : table = [[T(1,Rouge);T(2,Rouge);T(3,Rouge)];[T(4,Rouge);T(5,Rouge)]];;
let pioche : pioche = [(Joker, 1)];;
let cst_etat:etat = ((statut_1,statut_2), table, pioche, J1);;

let joueur_courant (etat:etat):joueur =
  match etat with
    |(_,_,_, J1) -> J1
    |(_,_,_, J2) -> J2
;;

(*Tests*)
assert (joueur_courant cst_etat = J1);;

let joueur_suivant (etat:etat):joueur =
  match etat with
    |(_,_,_, J1) -> J2
    |(_,_,_, J2) -> J1
;;
(*Tests*)
assert (joueur_suivant cst_etat = J2);;

let la_table (etat:etat):table =
  let (statut, table, pioche, joueur) = etat in
    table
;;
(*Tests*)
assert (la_table cst_etat = table);;

let la_pioche (etat:etat):pioche =
  let (statut, table, pioche, joueur) = etat in
    pioche
;;
(*Tests*)
assert (la_pioche cst_etat = pioche);;

let statut (etat: etat) (joueur: joueur) : statut =
  let (statuts, _, _, _) = etat in
  let (statut1, statut2) = statuts in
    if joueur = J1 then statut1 else statut2
;;

(*Tests*)
assert (statut cst_etat J1 = statut_1) ;;
assert (statut cst_etat J2 = statut_2) ;;

let la_main (etat:etat) (joueur:joueur):main =
  let (statut, _, _ , _) = etat in
  let ((joueur1, condition1, main1),(joueur2, condition2, main2)) = statut in
    if joueur = joueur2 then main2
    else main1
;;
(*Tests*)
assert (la_main cst_etat J1 = [(T (2, Rouge), 1);(T (1, Jaune), 2); (T (2, Jaune), 1)]);;
assert (la_main cst_etat J2 = [(T (2, Bleu), 1);(T (1, Bleu), 2); (T (2, Bleu), 1)]);;

(*q10*)
let est_suite (c:combinaison) =
  let rec aux ((valeur:int),(couleur:couleur))(occ:int)(c:combinaison):bool =
    match c with
      | [] -> occ>=3
      | Joker :: rest -> aux ((valeur+1),couleur) (occ+1) rest
      | T(valeur_n, couleur_n)::rest -> 
          if valeur_n = valeur +1 && couleur = couleur_n then aux (valeur_n, couleur_n) (occ+1) rest
          else false 
  in 
    match c with
      | [] -> true
      | [Joker]|[Joker;Joker]-> false
      | Joker::T(valeur,couleur)::rest -> aux (valeur, couleur) 2 rest
      | Joker::Joker::T(valeur, couleur)::rest -> aux (valeur, couleur) 3 rest 
      | T(valeur, couleur)::rest -> aux (valeur, couleur) 1 rest
      | _ -> false
;;

(*Tests*)
assert (est_suite [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)]);;
assert_not (est_suite [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(4,Rouge)]);;
assert (est_suite [Joker;T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)]);;

let rec appartient_combi x c = 
  match c with
    |[]-> false
    |debut::fin -> x=debut || (appartient_combi x fin)
;;

let est_groupe (c:combinaison) =
  let rec aux (occ:int)(valeur:int)(couleur_l:couleur list)(c:combinaison):bool =
    match c with
      | [] -> occ = 3 || occ = 4
      | Joker::rest -> aux (occ+1) valeur couleur_l rest
      | T(valeur_n, couleur_n)::rest -> 
          if (not(appartient_combi couleur_n couleur_l) && valeur = valeur_n) 
          then aux (occ+1) valeur (couleur_n::couleur_l) rest
          else false
  in 
    match c with
      | [] -> true
      | [Joker]|[Joker;Joker]-> false
      | Joker::T(valeur,couleur)::rest -> aux 2 valeur [couleur] rest
      | Joker::Joker::T(valeur, couleur)::rest -> aux 3 valeur [couleur] rest
      | T(valeur, couleur)::rest -> aux 1 valeur [couleur] rest
      | _ -> false
;;

(*Tests*)
assert (est_groupe [T(12,Rouge);T(12,Jaune);T(12,Bleu)]);;
assert_not (est_groupe [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(4,Rouge)]);;
assert (est_groupe [T(12,Rouge);T(12,Jaune);T(12,Bleu);Joker]);;

let combinaison_valide (c:combinaison) =
  est_groupe c || est_suite c
;;
(*Tests*)
assert (combinaison_valide [T(12,Rouge);T(12,Jaune);T(12,Bleu)]);;
assert (combinaison_valide [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)]);;

let rec combinaisons_valides (cl:combinaison list) =
  match cl with
    | [] -> true
    | c::rest -> combinaison_valide c && combinaisons_valides rest
;;
(*Tests*)
assert (combinaisons_valides
          [[T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)];[T(12,Rouge);T(12,Jaune);T(12,Bleu)]]);;
