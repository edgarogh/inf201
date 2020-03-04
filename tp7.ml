(* TP7 - DECORSAIRE Matteo & ONGHENA Edgar *)

type monome = int * int;;
type polynome = monome list;;

let cst_M1: monome = (10, 0);;
let cst_M2: monome = (7, 1);;
let cst_M3: monome = (-3, 2);;
let cst_M4: monome = (1, 4);;
let cst_M5: monome = (-1, 4);;

let cst_P1: polynome = [cst_M1];;
let cst_P2: polynome = [cst_M2; cst_M1];;
let cst_P3: polynome = [cst_M3; cst_M2];;
let cst_P4: polynome = [cst_M4; cst_M3; cst_M2];;
let cst_P5: polynome = [cst_M5];;

let derive_monome ((coeff, puiss): monome) : monome =
    let coeff = coeff * puiss
    and puiss = puiss - 1
    in
        (coeff, if coeff = 0 then 0 else puiss)
;;

assert ((0, 0) = derive_monome (0, 0));;
assert ((0, 0) = derive_monome cst_M1);;
assert ((-6, 1) = derive_monome cst_M3);;

let rec derive_polynome (p: polynome) : polynome =
    match p with
        | [] -> []
        | monome :: reste -> (
            let monome_derive = derive_monome monome in
                if monome_derive = (0, 0) then
                    derive_polynome reste
                else
                    monome_derive :: derive_polynome reste
        )
;;

assert ([] = derive_polynome [cst_M1]);;
assert ([(7, 0)] = derive_polynome [cst_M2]);;
assert ([(-6, 1)] = derive_polynome [cst_M3]);;
assert ([(4, 3)] = derive_polynome [cst_M4]);;
assert ([(-4, 3)] = derive_polynome [cst_M5]);;

assert ([] = derive_polynome cst_P1);;
assert ([(7, 0)] = derive_polynome cst_P2);;
assert ([(-6, 1); (7, 0)] = derive_polynome cst_P3);;
assert ([(4, 3); (-6, 1); (7, 0)] = derive_polynome cst_P4);;

let degre_polynome (p: polynome) : int =
    match p with
        | [] -> 0
        | (_, plus_grande_puissance) :: _ -> plus_grande_puissance
;;

(* Retourne le coeff correspondant à une certaine puissance, dans un polynome *)
(*
 Exemple: avec le polynôme "4x^6 + 3x^2" et la puissance 2, retourne 3
*)
let rec coeff_pour_puissance (p: polynome) (puiss_cherchee: int) : int =
    match p with
        | [] -> 0
        | (coeff, puiss) :: reste -> (
            if puiss = puiss_cherchee then
                coeff
            else
                coeff_pour_puissance reste puiss_cherchee
        )
;;

(*
 ALGORITHME:
  - On cherche la puissance max
  - De façon récursive, en partant de cette puissance max on trouve les coeffs 
   correspondant à cette puissance pour chacun des polynômes, on les 
   additionne et on créer un nouveau monome somme, et on procède de même pour 
   les monomes suivants
*)
let somme_polynome (a: polynome) (b: polynome) : polynome =
    let rec somme_partie (puiss: int) : polynome =
        if puiss < 0 then
            []
        else
            let coeff_a = coeff_pour_puissance a puiss
            and coeff_b = coeff_pour_puissance b puiss
            in
                let monome = (coeff_a + coeff_b, puiss) in
                    if 0 = fst monome then
                        somme_partie (puiss - 1)
                    else
                        monome :: somme_partie (puiss - 1)
    in
        let max_degre = max (degre_polynome a) (degre_polynome b) in
            somme_partie max_degre
;;

(*
 Partie non demandée: exercices / utilitaires
 Utilisations de fonctions standard
*)

let string_of_monome ((coeff, puiss): monome) : string =
    let str_coeff = (
        match coeff with
            | 0 -> failwith "Unhandled null monome"
            | 1 -> ""
            | coeff when coeff < 0 -> "(" ^ (string_of_int coeff) ^ ")"
            |_ -> string_of_int coeff
    )
    and str_x = if puiss <> 0 then "x" else ""
    and str_exp = if puiss > 1 then "^" ^ string_of_int puiss else ""
    in
        str_coeff ^ str_x ^ str_exp
;;

let string_of_polynome (p: polynome) : string =
    String.concat " + " (List.map string_of_monome p)
;;

print_string (string_of_polynome (somme_polynome cst_P5 cst_P1));;
