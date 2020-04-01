(**
    dernier_elt

    PROFIL:
     * dernier_elt: 'a list -> 'a

    SEMANTIQUE:
     * (dernier_elt s) donne l'élément le plus à droite de s

    EXEMPLES:
     - ∀ e ∈ 'a, (dernier_elt [e]) = e
     - (dernier_elt [5.1; 5.2; 5.3]) = 5.3
     - (dernier_elt ('a' :: 'b' :: 'c' :: []))
       = (dernier_elt ['a'; 'b'; 'c'])
       = 'c'
     - (dernier_elt ([[1; 2; 3] ; [3; 8; 7] ; [] ; [123]])) = [123]

    EQUATIONS:
     - (dernier_elt (a :: [])) = a
     - (dernier_elt (a :: reste)) = ({récursif} dernier_elt reste)

    TERMINAISON:
     - `let mesure = List.length` -> c'est bien décroissant
*)
let rec dernier_elt = function
    | [] -> failwith "Liste vide (@ dernier_elt)"
    | dernier :: [] -> dernier
    | _ :: reste -> dernier_elt reste

let () =
    assert (1 = dernier_elt [1]);
    assert (4 = dernier_elt [1; 2; 3; 4]);


type text = char list

(*
    app_e

    PROFIL:
     * app_e: text -> bool

    SEMANTIQUE:
     * (app_e t) renvoie vrai si 'e' ∈ t
*)

(*
    Q2
    (app_e ('e' :: _)) = true
    (app_e []) = false
    (app_e el :: reste) = app_e reste
*)

(*
    Q3
    La mesure doit prendre les mêmes arguments que la fonction. Elle doit être
    décroissante pour deux appels successifs de n'importe quelle récursion.
    La fonction se termine car pour chaque itération, la liste est plus petite
    d'un élément, la mesure aussi. Quand la liste est vide, elle retourne de
    façon non-récursive.
*)

(* Filtrage & conditions *)
let rec app_e_v1 = function
    | [] -> false
    | 'e' :: _ -> true
    | _ :: reste -> app_e_v1 reste

(* Filtrage & expr. booléenne *)
let rec app_e_v2 = function
    | [] -> false
    | el :: reste -> (el = 'e') || (app_e_v2 reste)

(* Expr. booléenne uniquement (sans filtrage explicite) *)
let rec app_e_v3 = List.fold_left (fun acc -> fun el -> el = 'e' || acc) false


(*
    app_car

    PROFIL:
     * app_car: char -> text -> bool

    SEMANTIQUE:
     * (app_car c t) renvoie vrai si c ∈ t
*)

let mesure_app_car = List.length

let rec app_car (c_attendu: char) = function
    | [] -> false
    | c :: _ when c = c_attendu -> true
    | _ :: reste -> app_car c_attendu reste


let rec app attendu = function
    | [] -> false
    | c :: _ when c = attendu -> true
    | _ :: reste -> app attendu reste


let app_e = app 'e'
