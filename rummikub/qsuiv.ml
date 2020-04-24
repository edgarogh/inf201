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

let rec cardinal (mens: 'a multiensemble) : int =
  match mens with
    | [] -> 0
    | (_, occurences) :: rest -> occurences + cardinal rest
;;

(*Tests*)
assert (0 = cardinal []);;
assert (3 = cardinal cst_mens2);;
assert (6 = cardinal cst_mens1);;

let rec nbocc (which: 'a) (mens: 'a multiensemble) : int =
  match mens with
    | [] -> 0
    | (el, occurences) :: rest -> (
        if el = which then occurences else nbocc which rest
      )
;;

(*Tests*)
assert (3 = nbocc 'a' cst_mens1);;
assert (1 = nbocc 'c' cst_mens1);;

let rec appartient (which: 'a) (mens: 'a multiensemble) : bool =
  (nbocc which mens) > 0
;;

(*Tests*)
assert (appartient 'b' cst_mens1);;
assert_not (appartient 'd' cst_mens1);;

let rec inclus (mens1: 'a multiensemble) (mens2: 'a multiensemble) : bool =
  match mens1 with
    | [] -> true (* L'ensemble vide est dans tous les ensembles *)
    | (el, occurences) :: rest -> (
        (occurences <= nbocc el mens2)
        &&
        (inclus rest mens2)
      )
;;

(*Tests*)
assert (inclus cst_mens2 cst_mens1);;
assert_not (inclus cst_mens1 cst_mens2);;

let rec ajoute ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
  if appartient n_value mens then (* Element présent, on mappe en modifiant l'élément *)
    match mens with
      | [] -> []
      | (value, occurences) :: rest when (n_value = value) -> (
          let mel_sum : 'a multielement = (value, occurences + n_occ) in
            if (snd mel_sum) <= 0 then (* Peut se produire lors d'une suppression *)
              rest
            else
              mel_sum :: rest
        )
      | mel :: rest -> let a = ajoute (n_value, n_occ) rest in
            mel :: a
  else (* Elément pas présent, on le rajoute au début tel quel *)
    (n_value, n_occ) :: mens
;;


assert (cst_mens1 = (ajoute ('a', 2) (ajoute ('b', 1) cst_mens2)));;
assert ([('a', 1); ('b', 1)] = (ajoute ('c', -1) cst_mens2));;

let supprime ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
  ajoute (n_value, -n_occ) mens
;;

assert ([('a', 1); ('b', 1)] = (supprime ('c', 1) cst_mens2));;
assert ([('a', 3); ('c', 1)] = (supprime ('b', 4) cst_mens1));;

let egaux (a: 'a multiensemble) (b: 'a multiensemble) : bool =
  (inclus a b) && (inclus b a) (* Double inclusion *)
;;

assert (egaux cst_mens1 cst_mens1);;
assert_not (egaux cst_mens1 cst_mens3);;
assert_not (egaux cst_mens1 cst_mens2);;

let rec intersection (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
  match a with
    | [] -> []
    | (value, occurences) :: rest -> (
        let in_common_count = (min occurences (nbocc value b)) in
          if in_common_count = 0 then
            intersection rest b
          else
            (value, in_common_count) :: (intersection rest b)
      )
;;

assert (cst_mens2 = intersection cst_mens1 cst_mens2);;
assert ([('a', 3)] = intersection cst_mens1 ([('a', 4)]));;

let rec difference (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
  match b with
    | [] -> a (* A - Ø = A *)
    | mel :: rest -> (difference (supprime mel a) rest)
;;

assert (([('a', 2); ('b', 1)]) = difference cst_mens1 cst_mens2);;

let rec ieme (n: int) (mens: 'a multiensemble) : 'a multielement =
  if n < 0 then
    failwith "n < 0"
  else
    match mens with
      | [] -> failwith "n >= cardinal mens"
      | (value, occurences) :: rest -> (
          if n < occurences then
            (value, 1)
          else
            ieme (n - occurences) rest
        )
;;

assert (('a', 1) = ieme 1 cst_mens1);;
assert (('b', 1) = ieme 4 cst_mens1);;
assert (('c', 1) = ieme 5 cst_mens1);;

let un_dans (mens: 'a multiensemble) : 'a multielement =
  let length = cardinal mens in
    ieme (Random.int length) mens
;;

(* Suite de test manuelle pour "un_dans" *)

let ens = [ ('a', 2); ('b', 1)];;

(*
Pour un nombre de "lancés" "remaining_rolls", retourne le nombre de fois que
chaque lettre 'a'/'b' est tombée. Il devrait statistiquement il y avoir deux
fois plus de 'a' que de 'b'. Comme ce test n'est pas déterministe du tout,
il revient à l'utilisateur de vérifier au jugé, d'après ce qui s'affiche dans
la console, si la distribution semble conforme.
*)
(*let rec test_un_dans (remaining_rolls: int) : int * int =
  if remaining_rolls = 0 then (0, 0)
  else
  let roll = un_dans ens (* "choix" actuel pour cette itération *)
  and (a, b) = test_un_dans (remaining_rolls - 1) (* Données des itérations suivantes *)
  in
  if roll = 'a'
  then (a + 1, b)
  else (a, b + 1)
  ;;

  let (a, b) = (test_un_dans 10000) in (
  print_string "a/b (doit être proche de 2): ";
  print_float ((float_of_int a) /. (float_of_int b));
  print_newline ();
  );;*)

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

assert (tri_insertion [T(1,Noir), 3; T(1,Noir), 2; T(1,Noir), 1] = [T(1,Noir), 1; T(1,Noir), 2; T(1,Noir), 3]);;

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
        [(T (1, Rouge), 2); (T (2, Rouge), 1);
         (T (1, Jaune), 2); (T (2, Jaune), 1);
         (T (1, Bleu), 2); (T (2, Bleu), 1); 
         (T (1, Noir), 2); (T (2, Noir), 3);
         (Joker, 2)]);;

type joueur = J1 | J2;;
type statut = joueur * bool * main;;
type etat = (statut * statut) * table * pioche * joueur;;
(*definir cst etat*)
(*q8*)
let rec extraire (n:int)(pioche:'a multiensemble):('a multiensemble*'a multiensemble)= 
  match n with
    | 1 -> let aleatoire:'a multielement = un_dans pioche in
        let mens:'a multiensemble = [aleatoire] in
          (mens,pioche)
    | _ -> 
        let (main_n,pioche_n) = extraire (n-1) pioche in 
        let aleatoire = un_dans pioche in
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

let joueur_courant (etat:etat):joueur =
  match etat with
    |(_,_,_, J1) -> J1
    |(_,_,_, J2) -> J2
;;

let joueur_suivant (etat:etat):joueur =
  match etat with
    |(_,_,_, J1) -> J2
    |(_,_,_, J2) -> J1
;;

let la_table (etat:etat):table =
  let (statut, table, pioche, joueur) = etat in
    table
;;

let la_pioche (etat:etat):pioche =
  let (statut, table, pioche, joueur) = etat in
    pioche
;;

let statut (etat:etat) (joueur:joueur):statut =
  let (statut, table, pioche, joueur) = etat in
  let ((joueur1, condition1, main1),(joueur2, condition2, main2)) = statut in
    if joueur = joueur2 then (joueur2, condition2, main2) 
    else (joueur1, condition1, main1)
;;

let la_main (etat:etat) (joueur:joueur):main =
  let (statut, table, pioche, joueur) = etat in
  let ((joueur1, condition1, main1),(joueur2, condition2, main2)) = statut in
    if joueur = joueur2 then main2
    else main1
;;

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

assert (est_groupe [T(12,Rouge);T(12,Jaune);T(12,Bleu)]);;
assert_not (est_groupe [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(4,Rouge)]);;
assert (est_groupe [T(12,Rouge);T(12,Jaune);T(12,Bleu);Joker]);;

let combinaison_valide (c:combinaison) =
  est_groupe c || est_suite c
;;

let rec combinaisons_valides (cl:combinaison list) =
  match cl with
    | [] -> true
    | c::rest -> combinaison_valide c && combinaisons_valides rest
;;

