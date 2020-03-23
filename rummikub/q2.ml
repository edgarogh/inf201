(* Specification: assertion inverse *)
let assert_not cond = assert (not cond);;


type 'a multielement = 'a * int;;
type 'a multiensemble = 'a list;;

let cst_mens1: char multiensemble = ['a'; 'a'; 'a'; 'b'; 'b'; 'c'];;
let cst_mens2: char multiensemble = ['a'; 'b'; 'c'];;
let cst_mens3: char multiensemble = ['c'; 'a'; 'a'; 'a'; 'b'; 'b'];;

let rec cardinal (mens: 'a multiensemble) : int =
    match mens with
        | [] -> 0
        | _ :: rest -> 1 + cardinal rest
;;

assert (0 = cardinal []);;
assert (3 = cardinal cst_mens2);;
assert (6 = cardinal cst_mens1);;

let rec nbocc (which: 'a) (mens: 'a multiensemble) : int =
    match mens with
        | [] -> 0
        | el :: rest -> (
            (nbocc which rest)
            +
            (if el = which then 1 else 0)
        )
;;

assert (3 = nbocc 'a' cst_mens1);;
assert (1 = nbocc 'c' cst_mens1);;

let rec appartient (which: 'a) (mens: 'a multiensemble) : bool =
    (nbocc which mens) > 0
;;

assert (appartient 'b' cst_mens1);;
assert_not (appartient 'd' cst_mens1);;

(* Fonction auxiliaire *)
let rec inclus_mel ((value, occ): 'a multielement) (mens: 'a multiensemble) : bool =
    match mens with
        | [] -> occ = 0
        | el :: reste -> (
            if el = value then
                inclus_mel (value, occ - 1) mens
            else
                inclus_mel (value, occ) mens
        )
;;

let rec inclus (mens1: 'a multiensemble) (mens2: 'a multiensemble) : bool =
    match mens1 with
        | [] -> true (* L'ensemble vide est dans tous les ensembles *)
        | A((el, occurences), rest) -> (
            (occurences <= nbocc el mens2)
            &&
            (inclus rest mens2)
        )
;;

assert (inclus cst_mens2 cst_mens1);;
assert_not (inclus cst_mens1 cst_mens2);;

let rec ajoute ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
    if n_occ = 0 then
        n_value :: (ajoute (n_value, n_occ - 1) mens)
    else
        mens
;;

assert (cst_mens1 = (ajoute ('a', 2) (ajoute ('b', 1) cst_mens2)));;

let rec supprime ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
    match mens with
        | [] -> (
            if n_occ <> 0 then failwith "Element non inclus" else []
        )
        | el :: reste -> (
            if el = n_value then
                (supprime (n_value, n_occ - 1) reste)
            else
                el :: (supprime (n_value, n_occ) reste)
        )
;;

assert (['a', 'b'] = (supprime ('c', 1) cst_mens2));;

let egaux (a: 'a multiensemble) (b: 'a multiensemble) : bool =
    (inclus a b) && (inclus b a) (* Double inclusion *)
;;

assert (egaux cst_mens1 cst_mens1);;
assert (egaux cst_mens1 cst_mens3);;
assert_not (egaux cst_mens1 cst_mens2);;

let rec intersection (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    match a with
        | V -> V
        | A((value, occurences), rest) -> (
            let in_common_count = (min occurences (nbocc value b)) in
                if in_common_count = 0 then
                    intersection rest b
                else
                    A((value, in_common_count), intersection rest b)
        )
;;

assert (cst_mens2 = intersection cst_mens1 cst_mens2);;
assert (A(('a', 3), V) = intersection cst_mens1 (A(('a', 4), V)));;

let rec difference (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    match b with
        | V -> a (* A - Ø = A *)
        | A(mel, rest) -> (difference (supprime mel a) rest)
;;

assert (['a', 'a', 'b'] = difference cst_mens1 cst_mens2);;

let rec ieme (n: int) (mens: 'a multiensemble) : 'a =
    match mens with
        | [] -> failwith "n >= cardinal mens"
        | el :: [] when n = 0 -> el
        | el :: reste -> (ieme (n - 1) reste)
;;

assert ('a' = ieme 1 cst_mens1);;
assert ('b' = ieme 4 cst_mens1);;
assert ('c' = ieme 5 cst_mens1);;

let un_dans (mens: 'a multiensemble) : 'a =
    let length = cardinal mens in
        ieme (Random.int length) mens
;;

(* Suite de test manuelle pour "un_dans" *)

let ens = ['a', 'a', 'b'];;

(*
 Pour un nombre de "lancés" "remaining_rolls", retourne le nombre de fois que 
 chaque lettre 'a'/'b' est tombée. Il devrait statistiquement il y avoir deux 
 fois plus de 'a' que de 'b'. Comme ce test n'est pas déterministe du tout, 
 il revient à l'utilisateur de vérifier au jugé, d'après ce qui s'affiche dans 
 la console, si la distribution semble conforme.
*)
let rec test_un_dans (remaining_rolls: int) : int * int =
    if remaining_rolls = 0 then (0, 0)
    else
        let roll = un_dans ens (* "choix" actuel pour cette itération *)
        and (a, b) = test_un_dans (remaining_rolls - 1) (* Données des itérations suivantes *)
        in
            if roll = 'a'
            then (a + 1, b)
            else (a, b + 1)
;;

let (a, b) = (test_un_dans 10000) in (
    print_string "a/b (doit être proche de 2): ";
    print_float ((float_of_int a) /. (float_of_int b));
    print_newline ();
);;
