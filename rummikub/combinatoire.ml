open Multiensemble
open Tuiles

(**
Ce fichier rassemble les fonctions nécessaires à la Q15 (implémentées avec
l'algorithme 2).

L'essentiel du travail est d'arriver à énumérer toutes les possibilités de
combinaisons à partir d'une main donnée. Le déroulement de cet algorithme dans
notre implémentation est au juste au milieu entre optimisation et facilité de
lecture. Notre algorithme est très générique et l'essentiel du code est commun
aux suites et aux groupes.

L'idée générale est de générer, récursivement, une liste de toutes les
combinaisons possibles à partir de la main, et de les filtrer à l'aide d'un
prédicat parmis deux, selon si on cherche un groupe ou une suite.

En pratique, il semble impossible de faire ça pour des mains assez grandes, tant
le nombre de combinaisons possibles est immense. Pour pallier à ce problème,
notre solution est de filtrer - pendant leur génération - les combinaisons
"partielles" qui n'ont aucun moyen, par ajout par la droite, d'être transformées
en combinaisons valides.

De par leur construction, les comb. intermédiaires sont forcément valide dès
lors qu'elle sont suffisamment grandes (>= 3).

Exemple:
    (pour un groupe): T (1, Rouge); T (2, Rouge) -> Impossible de créer un
        groupe dès ce moment. Il est inutile de rajouter des tuiles derrière et
        de tester à chaque fois.
    (pour une suite): T (2, Bleu); T (3, Bleu); T (5, Bleu) -> Impossible de
        créer une suite valide par ajout par la droite à partir de là. La
        recursion s'arrête ici.

Terminologie:
    Combinaison partielle a.k.a combp: Un "début" de combinaison qui peut être,
        mais n'est pas forcément, valide.
    Predicat intermédiaire a.k.a pred_int: Predicat qui, d'après une
        combinaison partielle et une tuile, renvoie true si la tuile peut être
        ajoutée et la nouvelle combinaison forme toujours une combinaison
        (partielle?) valide (d'après les critères d'une suite ou d'un groupe).
*)

(*
    FONCTION AUXILIAIRE - Essentiellement expliquée ci-dessus

    Retourne toutes les combinaisons possibles pour une main donnée et un
    prédicat intermédiaire donné

    ALGO: Récursion pour générer les possiblités, avec filtrage par chacun des
    deux predicats à différents moments
*)
let combinaisons_possibles pred_int (main: main) =
    let rec completer_combp combp (main: main) : combinaison list =
        let tuiles_dans_main : tuile list = List.map fst main in
        let tuiles_valides = List.filter (pred_int combp) tuiles_dans_main in
        (* A partir des tuiles de la main qui peuvent être ajoutées à la
        combinaison `comb`, on forme des nouvelles combinaisons qui sont `comb`
        + la tuile, ainsi que des mains respectives sans la tuile qui a été
        ajoutée à la combinaison. *)
        let nouvelles_comb_mains : (combinaison * main) list = List.map (fun t ->
            combp @ [t],
            supprime (t, 1) main
        ) tuiles_valides in
        (* Reste à renvoyer les éventuelles combinaisons valides dès à présent
        ainsi que toutes les combinaisons dérivées de celles-ci (récursion) *)
        let combps =
            List.map fst nouvelles_comb_mains
            @
            List.flatten @@ List.map (fun (c, m) -> completer_combp c m) nouvelles_comb_mains
        in
        (* A partir de là, les combinaisons qui passent ne sont plus "partielles" *)
        List.filter (fun cbp -> List.length cbp >= 3) combps
    in
    completer_combp [] main
;;

(*
    Les fameux predicats intermédiaires, expliqués dans l'entête
    Type: tuile list -> tuile -> bool

    + au début, une petite fonction auxiliaire bonus
*)

(**
    FONCTION AUXILIAURE

    Renvoie la valeur attendue après la fin d'une suite sous la forme d'une
    option. `Some valeur` indique que l'élément après le dernier doit avoir le
    numéro spécifié, `None` indique qu'on ne peut pas déterminer de valeur
    (liste vide ou ne contenant que des jokers).
    La fonction part du principe qu'il y a au plus 2 jokers.
*)
let valeur_attendue_fin_suite combp : int option = match List.rev combp with
    | T (valeur, _) :: _ -> Some (valeur + 1)
    | Joker :: T (valeur, _) :: _ -> Some (valeur + 2)
    | Joker :: Joker :: T (valeur, _) :: _ -> Some (valeur + 3)
    | _ -> None
;;

assert (None = valeur_attendue_fin_suite []);;
assert (None = valeur_attendue_fin_suite [Joker]);;
assert (None = valeur_attendue_fin_suite [Joker; Joker]);;
assert (Some 4 = valeur_attendue_fin_suite [T (1, Rouge); Joker; Joker]);;
assert (Some 3 = valeur_attendue_fin_suite [Joker; T (1, Rouge); Joker]);;

let ajout_suite_valide combp tuile =
    let val_fin_attendue = valeur_attendue_fin_suite combp in
    match tuile with
    | Joker -> true
    | T (valeur, couleur) -> (
        (* Vrai si les tuiles ont la même couleur *)
        let couleur_ok = List.for_all (function
            | T (_, c) -> c = couleur
            | _ -> true
        ) combp in
        (* La valeur de la nouvelle tuile est bien celle attendue à la fin *)
        let valeur_ok = match val_fin_attendue with
            | Some v -> valeur = v
            | None -> true
        in
        (* On filtre des "débuts" impossibles *)
        let debut_ok = match combp with
        | Joker :: T (1, _) :: _ -> false
        | Joker :: Joker :: T (1, _) :: _ -> false
        | Joker :: Joker :: T (2, _) :: _ -> false
        | _ -> true
        in

        couleur_ok && valeur_ok && debut_ok
    )
;;

assert (ajout_suite_valide [] (T (5, Rouge)));;
assert (ajout_suite_valide [Joker] (T (5, Rouge)));;
assert (ajout_suite_valide [T (3, Rouge); Joker] (T (5, Rouge)));;
assert (ajout_suite_valide [T (3, Rouge); Joker; Joker] (T (6, Rouge)));;
assert_not (ajout_suite_valide [T (3, Rouge)] (T (4, Jaune)));;
assert_not (ajout_suite_valide [T (3, Rouge)] (T (3, Rouge)));;
assert_not (ajout_suite_valide [T (1, Rouge)] (T (1, Rouge)));;
assert_not (ajout_suite_valide [Joker; Joker; T (3, Rouge)] (T (1, Rouge)));;

let ajout_groupe_valide combp tuile =
    List.length combp < 4
    &&
    match tuile with
    | Joker -> true
    | T (valeur, couleur) ->
        (* Vrai s'il n'existe aucune tuile avec la couleur actuelle dans la combinaison *)
        let couleur_ok = not @@ List.exists (function
            | T (_, c) when c = couleur -> true
            | _ -> false
        ) combp in

        (* Vrai si la valeur correspond aux valeurs existantes *)
        let valeur_ok = match List.filter ((<>) Joker) combp with
        | T (v, _) :: _ -> v = valeur
        | [] -> true
        | Joker :: _ -> failwith "Unreachable" (* Inatteignable, Les Jokers sont censés être filtrés *)
        in

        couleur_ok && valeur_ok
;;

assert (ajout_groupe_valide [] (T (10, Rouge)));;
assert (ajout_groupe_valide [Joker; T (10, Jaune)] (T (10, Rouge)));;
assert_not (ajout_groupe_valide [Joker; T (10, Rouge)] (T (10, Rouge)));;

(* Fonctions "utiles" exportées *)

let suites_possibles = combinaisons_possibles ajout_suite_valide;;
let groupes_possibles = combinaisons_possibles ajout_groupe_valide;;
