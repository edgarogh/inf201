open Multiensemble
open Tuiles
open Combinatoire

let () = Random.self_init ();;

(*Types*)
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
    (*Fonction auxiliaire qui prend la tuile précédente et la suite de la combinaison*)
        match c with
        | [] -> occ >= 3 (* Condition : Plus de 3 tuiles *)
        | Joker :: rest -> aux ((valeur+1), couleur) (occ+1) rest (*Le Joker est valide de toute manière *)
        | T (valeur_n, couleur_n)::rest ->
            (*Conditions : les tuiles doivent se suivre et être de la même couleur*)
            if valeur_n = valeur + 1 && couleur = couleur_n then aux (valeur_n, couleur_n) (occ+1) rest
            else false
    in
    match c with
    (* On applique la fonctions auxiliaire aux différentes
    suites de tuiles qui apparraisent dans la combinaison *)
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

let rec appartient_combi (x:couleur) (c:couleur list) =
    match c with
    | [] -> false
    | debut :: fin -> x = debut || (appartient_combi x fin)
;;

(*Tests*)
assert (appartient_combi Jaune [Jaune; Noir]);;
assert_not (appartient_combi Jaune [Noir]);;

let est_groupe (c:combinaison) =
    let rec aux (occ: int) (valeur: int) (couleur_l: couleur list) (c: combinaison) : bool =
    (*Fonction auxiliaire qui prend la tuile précédente, la suite
    de la combinaison et la liste des couleurs déja rencontrées*)
    match c with
    | [] -> occ = 3 || occ = 4 (* Condition : 3 ou 4 tuiles *)
    | Joker :: rest -> aux (occ+1) valeur couleur_l rest
    | T(valeur_n, couleur_n) :: rest ->
        (* Conditions : Les tuiles doivent être de couleurs différentes et de même valeurs*)
        if (not (appartient_combi couleur_n couleur_l) && valeur = valeur_n)
        then aux (occ+1) valeur (couleur_n :: couleur_l) rest
        else false
    in
    match c with
    (* On applique la fonctions auxiliaire aux différentes
    suites de tuiles qui apparraisent dans la combinaison *)
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
    FONCTION AUXILIAIRE
    Renvoie la première valeur d'une suite, en tenant compte de l'éventualité
    qu'il s'agisse d'un Joker

    TYPE: combinaison -> int

    UTILISATION:
    - (debut_suite suite) renvoie la valeur de la première tuile de `suite`. Si
      cette tuile est un Joker, sa valeur est devinée à partir des tuiles
      suivantes

    EXEMPLES: (c.f. assertions)

    ALGO: Pattern matching
    Ne fonctionne qu'avec 2 Jokers maximum de par sa nature finie / non-recur.
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
    Somme des entiers consécutifs de m à n

    TYPE: int -> int -> int

    UTILISATION:
    - (somme_m_a_n m n) renvoie la somme des entiers consécutifs de m à n inclus

    EXEMPLES: (c.f. assertions)

    ALGO: Formule mathématique bien connue
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

(**
    FONCTION AUXILIAIRE
    Très explicite: créer un multiensemble à partir d'une liste

    TYPE: 'a list -> 'a multiensemble

    ALGO: Ordre supérieur + fonction "ajoute" des multiensembles, on ajoute des
    multielements de taille 1 pour chaque élément de la liste en partant d'un
    multiensemble vide.
*)
let multiensemble_of_list list =
    List.fold_left (fun acc -> fun el -> ajoute (el, 1) acc) [] list
;;

assert (egaux [('a', 2); ('b', 1)] (multiensemble_of_list ['a'; 'a'; 'b']));;

(* 8 *)
let tableVmens (table: table) : tuile multiensemble =
    let applati = List.flatten table in
    multiensemble_of_list applati
;;

assert (egaux [(Joker, 2); (T (1, Rouge), 1)] (tableVmens [[Joker; T (1, Rouge)]; [Joker]]));;

let premier_coup_ok (main: main) (pose: pose) (main_apres: main) =
    (
        (* Valeur totale des combinaisons >= 30 *)
        (points_pose pose) >= 30
    )
    &&
    (
        (* Transition cohérente *)
	    let pose = tableVmens pose in (* Conversion en mens *)
        egaux main_apres (difference main pose)
    )
;;

(* pose invalide, transition valide *)
assert_not (premier_coup_ok [] [] []);;
(* pose valide, transition invalide *)
assert_not (premier_coup_ok [(T (10, Jaune), 1)] [[T (10, Rouge); T (10, Rouge); T (10, Rouge)]] []);;
(* pose valide, transition valide *)
assert (premier_coup_ok [(T (10, Rouge), 3)] [[T (10, Rouge); T (10, Rouge); T (10, Rouge)]] []);;

let coup_ok (table0: table) (main0: main) (table1: table) (main1: main) =
    (
        (* Pose de cartes *)
        cardinal main0 > cardinal main1
    )
    &&
    (
        (* Transition cohérente *)
        let global0 = somme (tableVmens table0) main0 in
        let global1 = somme (tableVmens table1) main1 in
	    egaux global0 global1
    )
    &&
    (
        (* Table valide *)
        List.for_all combinaison_valide table1
    )
;;

let rec insere_tuile (tuile: tuile) (c: combinaison) : combinaison =
    match tuile with
    | T (n, cons) -> (
        match c with
            | [] -> T (n, cons) :: []
            | Joker :: queue -> Joker :: insere_tuile tuile queue
            | T(n_c, cons_c) :: queue ->
                (*Trier selon la valeur des tuiles*)
                if n < n_c then T (n, cons) :: c
                else T (n_c, cons_c) :: insere_tuile tuile queue
        )
    | Joker -> (
        match c with
            | [] -> Joker :: []
            | Joker :: queue -> Joker :: insere_tuile tuile queue
            | T(n_c, cons_c) :: queue -> T (n_c, cons_c) :: insere_tuile tuile queue
        )
;;

let rec tri_insertion_tuile = function
    | [] -> []
    | tete :: queue -> insere_tuile tete (tri_insertion_tuile queue)
;;

assert (tri_insertion_tuile [T (2, Noir); Joker; T (1, Noir)]= [T (1, Noir); Joker; T (2, Noir)]);;

let rec remplacer (r: combinaison) (c: combinaison) (table: table) : table =
    match table with
    | [] -> []
    | (x: combinaison) :: rest ->
        if x = c then r :: rest
        else x :: (remplacer r c rest)
;;

(*Tests*)
let c_1 : combinaison = [T (1, Rouge); T (2, Rouge); T (3, Rouge); T (4, Rouge); T (5, Rouge)];;
let c_1bis : combinaison = [T (1, Rouge); T (2, Rouge); T (3, Rouge); T (4, Rouge); T (5, Rouge); T (6, Rouge)];;
let c_2 : combinaison = [T (2, Noir); Joker; T (1, Noir)];;
let c_3 : combinaison = [T (2, Bleu); Joker; T (1, Jaune)];;

assert ((remplacer c_1bis c_1 [c_1; c_2]) = [c_1bis; c_2]);;

let rec ajouter_tuile (table: table) (tuile: tuile) : table =
    let rec ajouter_tuile_aux (c: combinaison) (tuile: tuile) : combinaison =
        (*verifie si une combinaison concaténée avec la tuile est valide*)
        let (combi: combinaison) = tri_insertion_tuile([tuile] @ c) in
            if (est_suite combi = true) || (est_suite combi = true) then combi
            else c
    in
    (*On applique la fonction aux sur toutes les combinaisons de la table*)
    match table with
        | [] -> []
        | x :: rest ->
            if (ajouter_tuile_aux x tuile = x)
            then ajouter_tuile rest tuile
            else let new_c : combinaison = (ajouter_tuile_aux x tuile) in
                (remplacer new_c x table)
;;

(*Tests*)
let test_table : table = [c_1; c_2; c_3];;
let result_table : table = [c_1bis; c_2; c_3];;
let test_tuile : tuile = T (6, Rouge);;
assert ((ajouter_tuile test_table test_tuile) = result_table);;

(**
    FONCTION AUXILIAIRE
    Retourne un élément aléatoire d'une liste, raise si la liste est vide

    TYPE: 'a list -> 'a

    Non déterministe (intestable)
*)
let un_dans_liste list =
    let len = List.length list in
    if len = 0
    then failwith "Empty list"
    else List.nth list (Random.int len)
;;

let extraction_suite main = un_dans_liste @@ suites_possibles main;;
let extraction_groupe main = un_dans_liste @@ groupes_possibles main;;


(**
    FONCTION AUXILIAIRE
    Modifie la main d'un joueur donné pour un état donné et renvoie le nouvel
    état. La modification se fait avec une fonction `f` qui prend une main et
    renvoie une nouvelle main.

    TYPE: etat -> joueur -> (main -> main) -> etat

    UTILISATION:
    - (modifier_main e J1 (supprime (Joker, 1))) renvoie une "copie" de l'état
      e où la main de J1 contient un joker de moins.
*)
let modifier_main etat joueur f : etat =
    let main = la_main etat joueur in
    let nouvelle_main = f main in

    let ((s11, s12, s13), (s21, s22, s23)), p1, p2, p3 = etat in
    let ss = if joueur = J1
        then ((s11, s12, nouvelle_main), (s21, s22, s23))
        else ((s11, s12, s13), (s21, s22, nouvelle_main))
    in
    (ss, p1, p2, p3)
;;


let piocher (e: etat) =
    let pioche = la_pioche e in
    if cardinal pioche = 0
    then e
    else
    let (stat1, stat2), table, _, joueur = e in

    let tuile_piochee = un_dans pioche in
    let status, _, _, _ = modifier_main e joueur (ajoute (tuile_piochee, 1)) in
    let nouvelle_pioche = supprime (tuile_piochee, 1) pioche in

    (status, table, nouvelle_pioche, joueur)
;;

(* La fonction n'est pas entièrement déterministe, tests difficiles à faire *)
let etat : etat = (((J1, false, [(Joker, 1)]), (J2, false, [])), [], [], J1) in
assert (etat = piocher etat);;
let etat0 : etat = (((J1, false, []), (J2, false, [(Joker, 1)])), [], [(Joker, 1)], J2) in
let etat1 : etat = (((J1, false, []), (J2, false, [(Joker, 2)])), [], [], J2) in
assert (etat1 = piocher etat0);;

(**
    FONCTION AUXILIAIRE
    Renvoie l'"autre" joueur, comme un "not" booléen

    TYPE: joueur -> joueur

    UTILISATION:
    - (autre joueur) renvoie l'autre joueur

    EXEMPLES:
    - J1 = autre J2
    - J2 = autre J1

    ALGORITHME: Pattern matching

    (j'exagère peut être la formalité ;-) )
*)
let autre = function J1 -> J2 | J2 -> J1;;

assert (J1 = autre J2);;
assert (J2 = autre J1);;


(**
    FONCTION AUXILIAIRE
    Renvoie un état où le joueur courant a été changé en l'autre joueur

    TYPE: etat -> etat
*)
let passer_tour (p0, p1, p2, joueur: etat) : etat =
    p0, p1, p2, autre joueur
;;


let jouer_1_coup (e: etat) (table_proposee: table) : etat =
    let _, table, _, joueur = e in
    let status_joueur = statut e joueur in
    let _, initialise, main = status_joueur in
    (* Initialisé: e *)
    if not initialise then e
    (* Table identique: pioche *)
    (*
    J'utilise la composition inverse pour rendre le code plus lisible. Elle se
    lit dans l'ordre dans lequel les fonction sont appliquées. Cela permet de
    chaîner des changement d'états proprement.
    *)
    else if table_proposee = table then e |> piocher |> passer_tour
    (* Table différente: enlever les cartes de la main *)
    else (
        (*
        Pour enlever de la main les tuiles ajoutées à la table, on ajoute
        l'ancienne table à la main puis on enlève la nouvelle table.
        *)
        let table_mels = List.map (fun x -> (x, 1)) (List.flatten table) in
        let e = modifier_main e joueur (fun main ->
            List.fold_left (fun acc -> fun e -> ajoute e acc) main table_mels
        ) in

        let table_p_mels = List.map (fun x -> (x, 1)) (List.flatten table_proposee) in
        let e = modifier_main e joueur (fun main ->
            List.fold_left (fun acc -> fun e -> supprime e acc) main table_p_mels
        ) in

        let p0, table, p2, p3 = passer_tour e in
        (p0, table_proposee, p2, p3)
    )
;;

let etat0 : etat = (((J1, true, [(T (2, Noir), 1)]), (J2, true, [])), [[T (2, Rouge); T (2, Jaune); T (2, Bleu)]], [], J1) in
let etat1 : etat = (((J1, true, []), (J2, true, [])), [[T (2, Rouge); T (2, Jaune); T (2, Bleu); T (2, Noir)]], [], J2) in
assert (etat1 = jouer_1_coup etat0 [[T (2, Rouge); T (2, Jaune); T (2, Bleu); T (2, Noir)]]);;


let jouer_1er_coup (e: etat) (pose: pose) : etat =
    let ((_, i1, _), (_, i2, _)), _, _, joueur = e in
    if joueur = J1 && i1 then e
    else
    if joueur = J2 && i2 then e
    else

    let main_avant = la_main e joueur in

    (* Suppression de la pose dans la main *)
    let pose_mels = List.map (fun x -> (x, 1)) (List.flatten pose) in
    let e = modifier_main e joueur (fun main ->
        List.fold_left (fun acc -> fun e -> supprime e acc) main pose_mels
    ) in

    let main_apres = la_main e joueur in

    if not @@ premier_coup_ok main_avant pose main_apres
    then (
        print_endline "Premier coup invalide";
        e
    )
    else
        let ((s11, s12, s13), (s21, s22, s23)), table, p2, _ = e in
        let status = match joueur with
        | J1 -> ((s11, true, s13), (s21, s22, s23))
        | J2 -> ((s11, s12, s13), (s21, true, s23))
        in
        (status, table @ pose, p2, autre joueur)
;;

let etat0 : etat = (((J1, false, [(T (10, Rouge), 2); (T (10, Jaune), 1); (T (10, Bleu), 1); (T (10, Noir), 1)]), (J2, true, [])), [], [], J1) in
let etat1 : etat = (((J1, true, [(T (10, Rouge), 1)]), (J2, true, [])), [[T (10, Rouge); T (10, Jaune); T (10, Bleu); T (10, Noir)]], [], J2) in
assert (etat1 = jouer_1er_coup etat0 [[T (10, Rouge); T (10, Jaune); T (10, Bleu); T (10, Noir)]]);;


(* === Scénario de jeu === *)

(* Inutile de le lancer, il y a toutes les chances pour qu'il ne marche pas *)
exit 0;;

let e = init_partie ();;

let e = piocher e;;
let e = piocher e;;

let e = jouer_1er_coup e [
    [T (9, Rouge); T (9, Jaune); T (9, Bleu)];
    [Joker; T (2, Jaune); T (3, Jaune)]
];;

let e = piocher e;;
let e = piocher e;;

let e = jouer_1er_coup e [
    [T (13, Jaune); T (13, Noir); T (13, Bleu)]
];;

let e = piocher e;;
let e = piocher e;;
let e = piocher e;;

let e = jouer_1_coup e [
    [T (9, Rouge); T (9, Jaune); T (9, Bleu); T (9, Noir)]; (* changement ici *)
    [Joker; T (2, Jaune); T (3, Jaune)];
    [T (13, Jaune); T (13, Noir); T (13, Bleu)]
];;

let e = piocher e;;
let e = piocher e;;

let e = jouer_1_coup e [
    [T (9, Rouge); T (9, Jaune); T (9, Bleu); T (9, Noir)];
    [Joker; T (2, Jaune); T (3, Jaune)];
    [Joker; T (13, Jaune); T (13, Noir); T (13, Bleu)] (* changement ici *)
];;

(* ... *)
