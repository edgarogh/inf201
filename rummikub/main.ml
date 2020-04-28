open Multiensemble
open Tuiles

type joueur = J1 | J2;;
type statut = joueur * bool * main;;
type etat = (statut * statut) * table * pioche * joueur;;

let distrib () =
    let (main1, pioche_inter) = extraire 14 cst_PIOCHE_INIT in
    let (main2, pioche) = extraire 14 pioche_inter in
    (main1, main2, pioche)
;;

let test = distrib ();;

let init_partie () : etat =
    let (main1, main2, pioche) = distrib () in
    let statut1 : statut = (J1, false, main1) in
    let statut2 : statut = (J2, false, main2) in
    let statut = (statut1, statut2) in
    (statut, [], pioche, J1)
;;

(*q9*)
(*constante de test*)
let statut_1 : statut = (J1, false, [(T (2, Rouge), 1); (T (1, Jaune), 2); (T (2, Jaune), 1)]);;
let statut_2 : statut = (J2, false, [(T (2, Bleu), 1); (T (1, Bleu), 2); (T (2, Bleu), 1)]);;
let table : table = [[T (1, Rouge); T (2, Rouge); T (3, Rouge)]; [T (4, Rouge); T (5, Rouge)]];;
let pioche : pioche = [(Joker, 1)];;
let cst_etat : etat = ((statut_1, statut_2), table, pioche, J1);;

let joueur_courant (_, _, _, joueur: etat) = joueur;;

(*Tests*)
assert (joueur_courant cst_etat = J1);;

let joueur_suivant (_, _, _, joueur: etat) : joueur =
    if joueur = J1 then J2 else J1
;;

(*Tests*)
assert (joueur_suivant cst_etat = J2);;

let la_table (_, table, _, _: etat) = table;;

(*Tests*)
assert (la_table cst_etat = table);;

let la_pioche (_, _, pioche, _: etat) = pioche;;

(*Tests*)
assert (la_pioche cst_etat = pioche);;

let statut (etat: etat) (joueur: joueur) : statut =
    let (statuts, _, _, _) = etat in
    let (statut1, statut2) = statuts in
    if joueur = J1 then statut1 else statut2
;;

(*Tests*)
assert (statut cst_etat J1 = statut_1);;
assert (statut cst_etat J2 = statut_2);;

let la_main (etat: etat) (joueur: joueur) : main =
    let (statut, _, _ , _) = etat in
    let ((joueur1, condition1, main1), (joueur2, condition2, main2)) = statut in
    if joueur = joueur2 then main2
    else main1
;;

(*Tests*)
assert (la_main cst_etat J1 = [(T (2, Rouge), 1);(T (1, Jaune), 2); (T (2, Jaune), 1)]);;
assert (la_main cst_etat J2 = [(T (2, Bleu), 1);(T (1, Bleu), 2); (T (2, Bleu), 1)]);;

(*q10*)
let est_suite (c: combinaison) =
    let rec aux ((valeur:int), (couleur: couleur)) (occ: int) (c: combinaison) : bool =
        match c with
        | [] -> occ >= 3
        | Joker :: rest -> aux ((valeur+1), couleur) (occ+1) rest
        | T (valeur_n, couleur_n)::rest ->
            if valeur_n = valeur + 1 && couleur = couleur_n then aux (valeur_n, couleur_n) (occ+1) rest
            else false
    in
    match c with
    | [] -> true
    | [Joker] | [Joker; Joker]-> false
    | Joker :: T (valeur, couleur) :: rest -> aux (valeur, couleur) 2 rest
    | Joker :: Joker :: T(valeur, couleur) :: rest -> aux (valeur, couleur) 3 rest
    | T (valeur, couleur) :: rest -> aux (valeur, couleur) 1 rest
    | _ -> false
;;

(*Tests*)
assert (est_suite [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)]);;
assert_not (est_suite [T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(4,Rouge)]);;
assert (est_suite [Joker;T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)]);;

let rec appartient_combi x c =
    match c with
    | [] -> false
    | debut :: fin -> x = debut || (appartient_combi x fin)
;;

let est_groupe (c:combinaison) =
    let rec aux (occ: int) (valeur: int) (couleur_l: couleur list) (c: combinaison) : bool =
    match c with
    | [] -> occ = 3 || occ = 4
    | Joker :: rest -> aux (occ+1) valeur couleur_l rest
    | T(valeur_n, couleur_n) :: rest ->
        if (not (appartient_combi couleur_n couleur_l) && valeur = valeur_n)
        then aux (occ+1) valeur (couleur_n :: couleur_l) rest
        else false
    in
    match c with
    | [] -> true
    | [Joker] | [Joker; Joker] -> false
    | Joker :: T(valeur, couleur) :: rest -> aux 2 valeur [couleur] rest
    | Joker :: Joker::T(valeur, couleur) :: rest -> aux 3 valeur [couleur] rest
    | T (valeur, couleur) :: rest -> aux 1 valeur [couleur] rest
    | _ -> false
;;

(*Tests*)
assert (est_groupe [T (12, Rouge); T (12, Jaune); T (12, Bleu)]);;
assert_not (est_groupe [T (1, Rouge); T (2, Rouge); T (3, Rouge); T (4, Rouge); T (4, Rouge)]);;
assert (est_groupe [T (12, Rouge); T (12, Jaune); T (12, Bleu); Joker]);;

let combinaison_valide (c: combinaison) = est_groupe c || est_suite c;;

(*Tests*)
assert (combinaison_valide [T (12, Rouge); T (12, Jaune); T (12, Bleu)]);;
assert (combinaison_valide [T (1, Rouge); T (2, Rouge); T (3, Rouge); T (4, Rouge); T (5, Rouge)]);;

let rec combinaisons_valides (cl: combinaison list) =
    match cl with
    | [] -> true
    | c :: rest -> combinaison_valide c && combinaisons_valides rest
;;

(*Tests*)
assert (combinaisons_valides
          [[T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)];[T(12,Rouge);T(12,Jaune);T(12,Bleu)]]);;



(* Edgar *)
(**
    FONCTION AUXILIAURE
    TODO
    Renvoie la première valeur d'une suite, en tenant compte de l'éventualité
    qu'il s'agisse d'un Joker
*)
let debut_suite : combinaison -> int = function
    | T (valeur, _) :: _ -> valeur
    | Joker :: T (valeur, _) :: _ -> valeur - 1
    | Joker :: Joker :: T (valeur, _) :: _ -> valeur - 2
    | _ -> failwith "Trop de Jokers en jeu ou combinaison invalide"
;;

assert (10 = debut_suite [T (10, Rouge)]);;
assert (10 = debut_suite [Joker; Joker; T (12, Rouge)]);;

(**
    FONCTION AUXILIAIRE
    TODO
    Somme des entiers consécutifs de m à n
*)
let somme_m_a_n m n = (n - m + 1) * (m + n) / 2;;

assert (5050 = somme_m_a_n 1 100);;
assert (21 = somme_m_a_n 10 11);;

let points_suite (c: combinaison) : int =
    let debut_suite = debut_suite c in
    let longueur_suite = List.length c in
    somme_m_a_n debut_suite (debut_suite + longueur_suite - 1)
;;

assert (6 = points_suite [T (1, Jaune); Joker; T (3, Jaune)]);;
assert (14 = points_suite [Joker; Joker; T (4, Jaune); T (5, Jaune)]);;

let points_groupe (c: combinaison) : int =
    let c_sans_joker = List.filter (function
        | Joker -> false
        | T (_, _) -> true
    ) c in
    match c_sans_joker with
    | T (valeur, _) :: _ -> valeur * (List.length c)
    | _ -> failwith "Une combinaison ne peut pas contenir que des Jokers"
;;

assert (3 = points_groupe [Joker; T (1, Jaune); Joker]);;

let points_pose (pose: pose) : int =
    let points (c: combinaison) =
        if est_suite c
        then points_suite c
        else points_groupe c
    in
    List.fold_left (+) 0 (List.map points pose)
;;
