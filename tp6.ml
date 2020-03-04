(* Noms : Onghena Edgar | Decorsaire Mattéo *)
(* Classe : IMA5 *)

(* TP6 : Fonctions récursives sur les entiers *)

type seq_int = ConsInt of int * seq_int | NilInt;;
type releve = seq_int;;

(*
 | Specification
 | nbj_sans
 | - Profil     : nbj_sans : releve -> int
 | - Semantique : (nbj_sans releve) est le nmbr de jours ou il n'y a aucune voiture.
 | - Exemples et propriétés :
 |   (a) nbj_sans (ConsInt(1,ConsInt(2,ConsInt(0,NilInt)))) = 1
 |   (b) nbj_sans (ConsInt(1,ConsInt(2,ConsInt(8,NilInt)))) = 0
 | Realisation
 |  - Algorithme : Fonction recursive avec un compteur 
*)

let rec nbj_sans r =
    match r with
        | NilInt -> 0
        | ConsInt(0, s) -> 1 + (nbj_sans s)
        | ConsInt(_, s) -> nbj_sans s
;;

let cst_R0 = nbj_sans (ConsInt(0,ConsInt(0,ConsInt(0,NilInt))));;
let cst_R1 = nbj_sans (ConsInt(1,ConsInt(2,ConsInt(3,NilInt))));;
let cst_R2 = nbj_sans (ConsInt(0,ConsInt(1,ConsInt(2,NilInt))));;
let cst_R3 = nbj_sans (ConsInt(1,ConsInt(0,ConsInt(0,NilInt))));;

(*
 | Specification
 | nbj_avec
 | - Profil     : nbj_avec :  int -> releve -> int
 | - Semantique : (nbj_avec releve x r) est le nmbr de jours ou il n'y a x voitures
 | - Exemples et propriétés :
 |   (a) nbj_avec 0 (ConsInt(0,ConsInt(0,ConsInt(0,NilInt)))) = 3
 |   (b) nbj_avec 5 (ConsInt(1,ConsInt(2,ConsInt(8,NilInt)))) = 0
 | Realisation
 |  - Algorithme : Fonction recursive avec un compteur 
*)

let rec nbj_avec (x: int) (r: releve) =
    match r with
        | NilInt -> 0
        | ConsInt(passages, reste) -> (
            (nbj_avec x reste) + if passages = x then 1 else 0
        )
;;

let acst_R0 = nbj_avec 1 (ConsInt(0,ConsInt(0,ConsInt(0,NilInt))));;
let acst_R1 = nbj_avec 3 (ConsInt(1,ConsInt(2,ConsInt(3,NilInt))));;
let acst_R2 = nbj_avec 2 (ConsInt(0,ConsInt(1,ConsInt(2,NilInt))));;
let acst_R3 = nbj_avec 1 (ConsInt(1,ConsInt(0,ConsInt(0,NilInt))));;

(*
 Q7

 Impossible d'effectuer une opération sur un nombre d'éléments arbitraires sans 
 récursion (ni boucles).
*)

(*
 | Specification
 | flux_app
 | - Profil     : flux_app : releve -> int -> bool
 | - Semantique : (flux_app x r) renvoie tru si x est dans r
 | - Exemples et propriétés :
 |   (a) flux_app 0 (ConsInt(0,ConsInt(0,ConsInt(0,NilInt)))) = true
 |   (b) flux_app 1 (ConsInt(7,ConsInt(2,ConsInt(8,NilInt)))) = false
 | Realisation
 |  - Algorithme : Fonction recursive avec ou logique
*)

let rec flux_app (x: int) (r: releve) : bool =
    match r with
        | NilInt -> false
        | ConsInt(pr, reste) -> (pr = x) || (flux_app x reste)
;;

#trace flux_app;;
flux_app 194 (ConsInt(125, ConsInt(142, ConsInt(253, ConsInt(194, ConsInt(155, ConsInt(45, ConsInt(62, NilInt))))))));;

(*
    flux_app <-- 194
    flux_app --> <fun>
    flux_app* <-- ConsInt (125, ConsInt (142, ConsInt (253, ConsInt (194, ConsInt (155, ConsInt (45, ConsInt (62, NilInt)))))))
        flux_app <-- 194
        flux_app --> <fun>
        flux_app* <-- ConsInt (142, ConsInt (253, ConsInt (194, ConsInt (155, ConsInt (45, ConsInt (62, NilInt))))))
            flux_app <-- 194
            flux_app --> <fun>
            flux_app* <-- ConsInt (253, ConsInt (194, ConsInt (155, ConsInt (45, ConsInt (62, NilInt)))))
                flux_app <-- 194
                flux_app --> <fun>
                flux_app* <-- ConsInt (194, ConsInt (155, ConsInt (45, ConsInt (62, NilInt))))
                flux_app* --> true
            flux_app* --> true
        flux_app* --> true
    flux_app* --> true
*)

(*
 4 éléments sont comparés. La fonction s'arrête dès qu'un élément correct est 
 trouvé, car l'opérateur `||` ne "s'embête pas" à évaluer l'opérande de droite 
 si celle de gauche est vraie.
*)


(* app générique *)

type 'a sequence = Element of 'a * 'a sequence | Nil;;

let rec app (x: 'a) (r: 'a sequence) : bool =
    match r with
        | Nil -> false
        | Element(pr, reste) -> (pr = x) || (app x reste)
;;

assert (app "hello" (Element("hello", Element("world", Nil))));;
assert (not (app "hello" (Element("bonjour", Element("world", Nil)))));;

assert (app 42 (Element(40, Element(41, Element(42, Nil)))));;
assert (not (app 43 (Element(40, Element(41, Element(42, Nil))))));;


(* Valeurs min et max *)

(*
 Le sujet veux que nous fassions une erreur mais nous ne savons pas laquelle...
 On saute les questions Q15-Q17
*)


type releveNV = ConsIntNV of int * releveNV | SingleIntNV of int;;

(*
 | Specification
 | fluxmin
 | - Profil     : fluxmin : releve -> int
 | - Semantique : (fluxmin r) renvoie le plus petit enregistrement du relevé
 | - Exemples et propriétés :
 |   (a) fluxmin (ConsIntNV(0,ConsIntNV(0,SingleIntNV(0)))) = 0
 |   (b) fluxmin (ConsIntNV(7,ConsIntNV(2,SingleIntNV(8)))) = 2
 | Realisation
 |  - Algorithme : Fonction recursive
*)

let rec fluxmin (r: releveNV) =
    match r with
        | SingleIntNV(element) -> element
        | ConsIntNV(element, reste) -> min element (fluxmin reste)
;;

assert (2 = fluxmin (ConsIntNV(7,ConsIntNV(2,SingleIntNV(8)))));;


let rec fluxmax (r: releveNV) =
    match r with
        | SingleIntNV(element) -> element
        | ConsIntNV(element, reste) -> max element (fluxmin reste)
;;

assert (8 = fluxmax (ConsIntNV(7,ConsIntNV(2,SingleIntNV(8)))));;
