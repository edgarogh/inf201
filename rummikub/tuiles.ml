open Multiensemble

(*q4*)
type valeur = int;;
type couleur = Rouge | Bleu | Jaune | Noir;;
type tuile = Joker | T of valeur * couleur;;

(*q5*)
type combinaison = tuile list;;
type table = combinaison list;;
type pose = combinaison list;;

(*q6*)
type main = tuile multiensemble;;
type pioche = main;;

let cst_PIOCHE_INIT : pioche =
    [
        (Joker, 2);
        T (1,Rouge), 2; T (2,Rouge), 2; T (3,Rouge), 2; T (4,Rouge), 2; T (5,Rouge), 2;
        T (6,Rouge), 2; T (7,Rouge), 2; T (8,Rouge), 2; T (9,Rouge), 2; T (10,Rouge), 2;
        T (11,Rouge), 2; T (12,Rouge), 2; T (13,Rouge), 2;
        T (1,Bleu), 2; T (2,Bleu), 2; T (3,Bleu), 2; T (4,Bleu), 2; T (5,Bleu), 2;
        T (6,Bleu), 2; T (7,Bleu), 2; T (8,Bleu), 2; T (9,Bleu), 2; T (10,Bleu), 2;
        T (11,Bleu), 2; T (12,Bleu), 2; T (13,Bleu), 2;
        T (1,Jaune), 2; T (2,Jaune), 2; T (3,Jaune), 2; T (4,Jaune), 2; T (5,Jaune), 2;
        T (6,Jaune), 2; T (7,Jaune), 2; T (8,Jaune), 2; T (9,Jaune), 2; T (10,Jaune), 2;
        T (11,Jaune), 2; T (12,Jaune), 2; T (13,Jaune), 2;
        T (1,Noir), 2; T (2,Noir), 2; T (3,Noir), 2; T (4,Noir), 2; T (5,Noir), 2;
        T (6,Noir), 2; T (7,Noir), 2; T (8,Noir), 2; T (9,Noir), 2; T (10,Noir), 2;
        T (11,Noir), 2; T (12,Noir), 2; T (13,Noir), 2
    ]
;;

let string_of_couleur = function
    | Rouge -> "R"
    | Bleu -> "B"
    | Jaune -> "J"
    | Noir -> "N"
;;

let string_of_tuile = function
    | Joker -> "Jk"
    | T (valeur, couleur) ->
        (string_of_int valeur) ^ (string_of_couleur couleur)
;;

(*q7*)
(*
    FONCTION AUXILIAIRE
    Filtre un multiensemble de tuiles pour ne garder qu'une couleur. Cette
    fonction, appliquée partiellement, permet de créer les fonctions `trouver_*`
    demandées dans l'énoncé.

    TYPE: couleur -> tuile multiensemble -> tuile multiensemble

    UTILISATION:
    - (trouver_couleur Jaune mens) renvoie un multiensemble qui contient toutes
      les tuiles jaunes de mens

    ALGO: Ordre superieur + Pattern matching
*)
let trouver_couleur couleur (mens: tuile multiensemble) : tuile multiensemble =
    List.filter (
        fun (el, occ) -> match el with
        | T (_, el_couleur) -> couleur = el_couleur
        | _ -> false
    ) mens
;;

let trouver_rouge = trouver_couleur Rouge;;

let trouver_jaune = trouver_couleur Jaune;;

let trouver_bleu = trouver_couleur Bleu;;

let trouver_noir = trouver_couleur Noir;;

let trouver_joker (mens: tuile multiensemble) =
    List.filter (fun (el,oc) -> el = Joker) mens
;;

(*tests*)
assert(trouver_rouge [ (Joker, 2); T(1,Rouge), 2; T(2,Bleu), 2 ] = [T(1,Rouge), 2]) ;;
assert(trouver_jaune [ (Joker, 2); T(1,Jaune), 2; T(2,Bleu), 2 ] = [T(1,Jaune), 2]) ;;
assert(trouver_bleu  [ (Joker, 2); T(1,Noir), 2; T(2,Bleu), 2 ]  = [T(2,Bleu), 2]) ;;
assert(trouver_noir  [ (Joker, 2); T(1,Noir), 2; T(2,Bleu), 2 ]  = [T(1,Noir), 2]) ;;
assert(trouver_joker [ (Joker, 2); T(1,Noir), 2; T(2,Bleu), 2 ]  = [(Joker, 2)]);;

let rec insere ((tuile, oc)) (mens : tuile multiensemble) =
    match tuile with
    | T (n, cons) -> (
        match mens with
            | [] -> (T (n, cons), oc) :: []
            | (Joker, _) :: [] -> failwith "impossible trier joker"
            | (Joker, _) :: _ -> failwith "impossible trier joker"
            | (T (n_c, cons_c),oc_n) :: queue ->
                if n < n_c then (T (n, cons), oc) :: mens
                else (T (n_c, cons_c), oc_n) :: insere (T (n, cons), oc) queue
        )
    | _ -> failwith "Je gère pas les jokers"
;;

let rec tri_insertion = function
    | [] -> []
    | tete :: queue -> insere tete (tri_insertion queue)
;;

assert (
    tri_insertion [T (1, Noir), 3; T (1, Noir), 2; T (1, Noir), 1]
    = [T (1, Noir), 1; T (1, Noir), 2; T (1, Noir), 3]
)
;;

let en_ordre (mens: tuile multiensemble) =
    tri_insertion (trouver_rouge mens) @
    tri_insertion (trouver_jaune mens) @
    tri_insertion (trouver_bleu mens)  @
    tri_insertion (trouver_noir mens)  @
    trouver_joker mens
;;

assert (
    en_ordre [
        T (2, Noir), 3; T (1, Noir), 2; T (2, Bleu), 1; T (1, Rouge), 2;
        T (2, Rouge), 1; Joker, 2; T (2, Jaune), 1; T (1, Jaune), 2;
        T (1, Bleu), 2;
    ] = [
        (T (1, Rouge), 2); (T (2, Rouge), 1);(T (1, Jaune), 2); (T (2, Jaune), 1);
        (T (1, Bleu), 2); (T (2, Bleu), 1); (T (1, Noir), 2); (T (2, Noir), 3); (Joker, 2)
    ]
)
;;

(*q8*)

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

let rec extraire (n: int) (pioche: 'a multiensemble) : 'a multiensemble * 'a multiensemble =
    match n with
    | 1 -> let aleatoire : 'a multielement = un_dans_mel pioche in
        let mens : 'a multiensemble = [aleatoire] in
        (mens, pioche)
    | _ ->
        let (main_n, pioche_n) = extraire (n-1) pioche in
        let aleatoire = un_dans_mel pioche in
        let main = en_ordre (aleatoire :: main_n) in
        (main, pioche)
;;

let test = extraire 4 cst_PIOCHE_INIT;;
