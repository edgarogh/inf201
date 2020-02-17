(* TP4 - DECORSAIRE Matteo & ONGHENA Edgar *)

(* 2.11.1 *)

(* Q1 *)

let poCoupleE ((x, y): int * int) =
    if x <= y then
        (x, y)
    else
        (y, x)
;;

(* Q2 *)
(*
 L'evaluation renvoie une erreur car le tuple n'accepte que des `int`, pas des 
 `float`.
*)

(* 2.11.2 *)

(* Q3 *)

2 < 3;; (* true *)
2.0 < 3.0;; (* true *)

(* Q4 *)

3 = 2;;
3.0 < 4.0;;
7 >= 2;;
7.0 <> 2.0;;

(* Q5 *)

let poCoupleR ((x, y)) =
    if x <= y then
        (x, y)
    else
        (y, x)
;;

(* 2.11.3 *)

(* Q6 *)

let poCouple (x, y: 'telem * 'telem) : 'telem * 'telem =
    if x <= y then
        (x, y)
    else
        (y, x)
;;

(* Q7 *)

poCouple (3,    2   );; (* renvoie un couple d'int *)
poCouple (33.3, 14.5);; (* renvoie un couple de float *)
(* poCouple (3,    14.5);; *) (* types différents, erreur *)

(* Q8 *)
(*
 'a -> 'a -> bool
 On le confirme en évaluant les expressions suivantes:
*)

(<);;
(>);;
(<=);;
(>=);;

(* Q9 *)
(*
 Les caractères sont comparés alphabétiquements par rapport à l'encodage 
 utilisé par ocaml (ascii ? utf-8 ?)
*)


(* TP4 - Type Durée et opérations associées *)

type de0a59 = int;; (* utilité / 20 ???? *)

type jour = int;;
type heure = int;;
type minute = int;;
type seconde = int;;

type duree = jour * heure * minute * seconde;;

let jour (j, h, m, s: duree)=j;;

let heure (j, h, m, s: duree)= h;;

let minute (j, h, m, s: duree)= m;;

let seconde (j, h, m, s: duree)=s;;

let div (a: int) (b: int) =
    (a / b, a mod b)
;;

let sec_en_duree (secondes: int) : duree =
    let (j, reste_j)=(div secondes (24 * 60 * 60)) in
        let (h, reste_h)=(div reste_j (60 * 60)) in
            let (m, s)=(div reste_h 60) in
                (j, h, m, s)
;;

assert ((0, 1, 0, 0) = sec_en_duree 3600);;
assert ((2, 1, 0, 0) = sec_en_duree ((24 * 2 * 3600) + 3600));;

let nb_total_sec (j: int) (h: int) (m: int) (s: int)
    = (j * 24 * 60 * 60)
    + (h * 60 * 60)
    + (m * 60)
    + (s)
;;

assert ((3600 * 24) = nb_total_sec 1 0 0 0);;
assert ((3600) = nb_total_sec 0 1 0 0);;
assert ((90) = nb_total_sec 0 0 1 30);;
assert ((120) = nb_total_sec 0 0 2 0);;

let vec_en_duree (j, h, m, s: int * int * int * int) =
    sec_en_duree (nb_total_sec j h m s)
;;

assert ((0, 2, 0, 0) = vec_en_duree (0, 1, 60, 0));;

let duree_en_sec (j, h, m, s: duree) = nb_total_sec j h m s;;

let som_duree_1 (d1: duree) (d2: duree) =
    sec_en_duree ((duree_en_sec d1) + (duree_en_sec d2))
;;

assert ((0, 1, 30, 0) = som_duree_1 (0, 0, 45, 0) (0, 0, 45, 0));;

let som_duree_2 (j1, h1, m1, s1: duree) (j2, h2, m2, s2: duree) : duree =
    let (rm, s)=(div (s1 + s2) (60)) in
        let (rh, m)=(div (m1 + m2 + rm) (60)) in
            let (rj, h)=(div (h1 + h2 + rh) (24)) in
                (rj + j1 + j2, h, m, s)
;;

assert ((0, 1, 30, 0) = som_duree_2 (0, 0, 45, 0) (0, 0, 45, 0));;

let eg_duree_1 (d1: duree) (d2: duree) =
    ((duree_en_sec d1) = (duree_en_sec d2))
;;

(* pour eg_duree_2 nous ne comprenons pas *)

let eg_duree_3 (j1, h1, m1, s1: duree) (j2, h2, m2, s2: duree) =
    (j1 = j2) && (h1 = h2) && (m1 = m2) && (s1 = s2)
;;

let eg_duree_4(d1: duree) (d2: duree) =
        ((jour d1) = (jour d2)) &&
        ((heure d1) = (heure d2)) &&
        ((minute d1) = (minute d2)) &&
        ((seconde d1) = (seconde d2))
;;

assert (eg_duree_4 (1,2,3,4) (1,2,3,4))

(* Q8 *)
let inf_duree_1(j1, h1, m1, s1: duree) (j2, h2, m2, s2: duree) =
    let a = duree_en_sec(j1, h1, m1, s1)
    and b = duree_en_sec(j2, h2, m2, s2) in
        b > a 
;;


let inf_duree_2(j1, h1, m1, s1: duree) (j2, h2, m2, s2: duree) =
    if j1=j2 then true else
    if h1=h2 then true else
    if m1=m2 then true else
    if s1=s2 then true else
    false
;;

(* TP4 - Codage des caractères *)

(* Q1 *)
(*
 La fonction n'accepte que des `char`s et retourne leur code ascii
*)

(* Q2 *)
(* On sait que 2⁸ - 1 = 255 et correspond au caractère maximal en terme d'ordre
en ASCII *)

(* Q3 *)
(* Profil : int --> char*)

(* Q4 *)
(* Profil : char --> int*)

(* Q5 - Q6*)
let chiffreVbase10(a: int) = char_of_int(a - int_of_char('0'));;
let base10Vchiffre(a: char) = int_of_char(a) - int_of_char('0');;

assert (chiffreVbase10 4 = '4');;
assert (base10Vchiffre '4' = 4);;

(* Numération en base 16 *)
type carhex = string;;
type base16 = int;;
