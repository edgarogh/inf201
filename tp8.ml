(*TP8 Onghena Edgar - Mattéo Decorsaire*)

(*Types*)
type chiffre = int (* 0 - 9 *)
type chiffreCar = char (* '1' - '9' *)
type nombre = chiffreCar list (* [’1’ ;’2’ ;’3’] *)
type txtnb = chiffreCar list (* [’ ’ ;’1’ ;’2’ ;’3’ ;’ ’] *)
type seqNombre = nombre list (*[ [’1’ ;’2’ ;’3’] ; [’4’ ;’5’] ; [’6’] ]*)

(*
 | Specification:
 | somme
 | - Profil     : somme : seqNombre -> int
 | - Semantique : (somme list) est la somme des entiers de la liste
 | - Exemples et propriétés :
 | somme ([123 ; 45 ; 6]) = 177
 | somme ([0 ; 0 ; 0]) = 0
*)
let somme : int list -> int = List.fold_left (+) 0

let ccVc (car: chiffreCar) : chiffre =
    (int_of_char car) - (int_of_char '0')

let () =
    assert (0 = ccVc '0');
    assert (5 = ccVc '5')

(*
 Fonction auxiliaire non demandée:
 convertit un char en une string de longueur 1

 Elle peut également être implémentée avec seulement des éléments du cours avec
 un match pour chaque chiffre (dans ce cas précis), mais elle ne marcherait plus
 pour tout caractère.
*)
let string_of_char = String.make 1

(*
 Fonction auxiliaire non demandée:
 Concatène une liste de string en une string
*)
let concat = List.fold_left (^) ""

let () =
    assert ("helloworld" = concat ["hello"; "world"])

let nbVnat (nb: nombre) : int =
    let seqChiffreString = List.map string_of_char nb in
    let nombre = concat seqChiffreString in
    int_of_string nombre

let () =
    assert (12 = nbVnat ['1'; '2'])

let snbVsnat : seqNombre -> int list =
    List.map nbVnat

let rec sup_esp (seq: txtnb) : txtnb =
    match seq with
        | ' ' :: rest -> sup_esp rest
        | other -> other

let () =
    assert (['a'; 'b'; ' '; 'c'] = sup_esp [' '; ' '; 'a'; 'b'; ' '; 'c'])

(*
 Nous avons largement dépassé le temps imparti en bloquant 2h sur prnb_r, on
 essaye de définir somme_txtnb mais absolument pas comme attendu dans le sujet
*)

let string_of_chars chars = concat (List.map string_of_char chars)

(*
 | Specification
 | somme_txtnb
 | - Profil     : somme_txtnb : txtnb -> int
 | - Semantique : (somme_txtnb nb) est la
 |                somme de nombres réprésentés en tant que txtnb
 | - Exemples et propriétés :
 |   somme_txtnb [’1’ ;’2’ ;’3’ ;’ ’ ;’ ’ ;’4’ ;’5’ ;’ ’ ;’6’] = 174
*)
let somme_txtnb (txt: txtnb) : int =
    let txt = string_of_chars txt in (* Concaténation des chars *)
    let parts = String.split_on_char ' ' txt in (* On coupe sur les espaces *)
    let parts = List.filter ((<>) "") parts in (* On enlève les "nombres" vides *)
    let numbers = List.map int_of_string parts in
    somme numbers

let () =
    assert (42 = somme_txtnb [' '; '3'; '2'; ' '; ' '; '1'; '0'])
