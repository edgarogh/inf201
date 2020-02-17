(*
 TP3
 DECORSAIRE Matteo
 ONGHENA Edgar
*)

(*
 -- Une date est-elle correcte ? --
*)

(* Q1 *)

type jour = int;;
type mois = int;;

let estJourDansMois_2 (j: jour) (m: mois) =
    (
        j >=1       (* Jour correct *)
        && m > 0    (* Mois correct *)
        && m <= 12  (*      ||      *)
    )
    && (
        ((m = 1 || m = 3 || m = 5 || m = 7 || m = 8 || m = 10 || m = 12) && j <= 31)
        || ((m = 2) && j <= 28)
        || ((m <> 2) && j <= 30)
    )
;;

assert (estJourDansMois_2 31 1);;
assert (not (estJourDansMois_2 30 2));;
assert (estJourDansMois_2 30 9);;
assert (not (estJourDansMois_2 31 9));;
assert (estJourDansMois_2 28 1);;

(*
 La fonction retourne false. La condition n'a pas de sens, donc on retourne 
 false volontairement.
*)

(* Q2 *)

let estJourDansMois_3 (j: jour) (m: mois) =
    j >= 1
    && match m with
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> j <= 31
        | 2 -> j <= 28
        | 4 | 6 | 9 | 11 -> j <= 30
        | _ -> false
;;

assert (estJourDansMois_3 31 1);;
assert (not (estJourDansMois_3 30 2));;
assert (estJourDansMois_3 30 9);;
assert (not (estJourDansMois_3 31 9));;
assert (estJourDansMois_3 28 1);;

(* Q3 *)

type mois_l = 
      Janvier
    | Fevrier
    | Mars
    | Avril
    | Mai
    | Juin
    | Juillet
    | Aout
    | Septembre
    | Octobre
    | Novembre
    | Decembre;;

let estJourDansMois_4 (j: jour) (m: mois_l) =
    j >= 1
    && match m with
        | Janvier | Mars | Mai | Juillet | Aout | Octobre | Decembre  -> j <= 31
        | Fevrier -> j <= 28
        | _ -> j <= 30
;;

(* (c)
 Le compilateur se plaint qu'on lui passe int au lieu d'un `mois_l`
*)

(* (d)
 assert ((estJourDansMois 0 Avril) = false)
 L'assertion passe car la fonction retourne la valeur attendue, soit false
*)


(*
 -- Relations sur des intervalles d'entiers --
*)

(* 2.9.1 *)

(* (10, 20, 30);; *)     (* Créer un tuple de trois entiers *)
(* ((10, 20), 30);; *)   (* Créer un 2-uplet qui contient un 2-uplet d'int et un int *)
(* 20, 30.0;; *)         (* Créer un 2-uplet *)
(* 4, 3 /. 2, 0;; *)     (* Erreur, on divise avec le mauvais opérateur *)
(* 4, 3. /. 2, 0;; *)    (* Erreur, on divise avec le mauvais opérateur *)
(* 4, 3. /. 2., 0;; *)   (* Créer un 3-uplet d'ints (4, 3/2, 0) *)
(* (4, 0) ./ (2, 0);; *) (* On ne peut pas diviser des tuples *)

(* 2.9.2 *)

type intervalle = int * int;;

let precede (n: int) ((bi, bs): intervalle) = n < bi;;
let suit    (n: int) ((bi, bs): intervalle) = n > bs;;
let dans    (n: int) ((bi, bs): intervalle) = (n >= bi) && (n <= bs);;

assert ((precede (-1) (0, 1)) = true);;
assert ((precede 0 (0, 1)) = false);;
assert ((suit 2 (0, 1)) = true);;
assert ((suit (-1) (0, 1)) = false);;
assert ((dans 0 (0, 1)) = true);;
assert ((dans (-1) (0, 1)) = false);;
assert ((dans 2 (0, 1)) = false);;

let intervalle_valide ((bi, bs): intervalle) = bi <= bs;;

let coteAcote ((bi1, bs1): intervalle) ((bi2, bs2): intervalle) =
    intervalle_valide (bi1, bs1)
    && intervalle_valide (bi2, bs2)
    && (
        bs1 = (bi2 - 1)
        || (bi1 - 1) = bs2
    )
;;

let chevauche ((bi1, bs1): intervalle) ((bi2, bs2): intervalle) =
    intervalle_valide (bi1, bs1)
    && intervalle_valide (bi2, bs2)
    && (
        (bi1 >= bi2 && bi1 <= bs2)
        || (bs1 >= bi2 && bs1 <= bs2)
    )
;;

let cst_i1 = (1, 3);;
let cst_i2 = (3, 4);;
let cst_i3 = (2, 8);;
let cst_i4 = (4, 10);;
let cst_i5 = (-1, 1);;

assert ((coteAcote cst_i1 cst_i4) = true);;
assert ((coteAcote cst_i3 cst_i5) = true);;
assert ((coteAcote cst_i1 cst_i2) = false);;
assert ((coteAcote cst_i3 cst_i2) = false);;

assert (true = chevauche cst_i1 cst_i2);;
assert (true = chevauche cst_i2 cst_i1);;
assert (false = chevauche cst_i1 cst_i4);;
assert (false = chevauche cst_i5 cst_i4);;

(*
 -- Somme des chiffres d'un nombre --
*)

let div (a: int) (b: int) =
    (a / b, a mod b)
;;

let sc (n: int) =
    let (m, reste_m)=(div n 1000) in
        let (c, reste_c)=(div reste_m 100) in
            let (d, u)=(div reste_c 10) in
                m + c + d + u
;;

assert (6 = sc 123);;
assert (8 = sc 4022);;
assert (1 = sc 100);;