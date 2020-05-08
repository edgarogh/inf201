(* Specification: assertion inverse *)
let assert_not cond = assert (not cond);;

(*Types*)
type 'a multielement = 'a * int;;
type 'a multiensemble = V | A of 'a multielement * 'a multiensemble;;

(*Constantes*)
let cst_mens1 = A(('a', 3), A(('b', 2), A(('c', 1), V)));;
let cst_mens2 = A(('a', 1), A(('b', 1), A(('c', 1), V)));;
let cst_mens3 = A(('c', 1), A(('a', 3), A(('b', 2), V)));;
let ens:char multiensemble = A(('a', 2), A(('b', 1), V));;

let rec cardinal (mens: 'a multiensemble) : int =
    match mens with
        | V -> 0
        | A((_, occurences), rest) -> occurences + cardinal rest
;;

(*Tests*)
assert (0 = cardinal V);;
assert (3 = cardinal cst_mens2);;
assert (6 = cardinal cst_mens1);;

let rec nbocc (which: 'a) (mens: 'a multiensemble) : int =
    match mens with
        | V -> 0
        | A((el, occurences), rest) -> (
            if el = which then occurences else nbocc which rest
        )
;;

(*Tests*)
assert (3 = nbocc 'a' cst_mens1);;
assert (1 = nbocc 'c' cst_mens1);;

let rec appartient (which: 'a) (mens: 'a multiensemble) : bool =
    (nbocc which mens) > 0
;;

(*Tests*)
assert (appartient 'b' cst_mens1);;
assert_not (appartient 'd' cst_mens1);;

let rec inclus (mens1: 'a multiensemble) (mens2: 'a multiensemble) : bool =
    match mens1 with
        | V -> true (* L'ensemble vide est dans tous les ensembles *)
        | A((el, occurences), rest) -> (
            (occurences <= nbocc el mens2)
            &&
            (inclus rest mens2)
        )
;;

(*Tests*)
assert (inclus cst_mens2 cst_mens1);;
assert_not (inclus cst_mens1 cst_mens2);;

let rec ajoute ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
    if appartient n_value mens then (* Element présent, on mappe en modifiant l'élément *)
        match mens with
            | V -> V
            | A((value, occurences), rest) when (n_value = value) -> (
                let mel_sum : 'a multielement = (value, occurences + n_occ) in
                    if (snd mel_sum) <= 0 then (* Peut se produire lors d'une suppression *)
                        rest
                    else
                        A(mel_sum, rest)
            )
            | A(mel, rest) -> A(mel, ajoute (n_value, n_occ) rest)
    else (* Elément pas présent, on le rajoute au début tel quel *)
        A((n_value, n_occ), mens)
;;

(*Tests*)
assert (cst_mens1 = (ajoute ('a', 2) (ajoute ('b', 1) cst_mens2)));;
assert (A(('a', 1), A(('b', 1), V)) = (ajoute ('c', -1) cst_mens2));;

let supprime ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
    ajoute (n_value, -n_occ) mens
;;

(*Tests*)
assert (A(('a', 1), A(('b', 1), V)) = (supprime ('c', 1) cst_mens2));;
assert (A(('a', 3), A(('c', 1), V)) = (supprime ('b', 4) cst_mens1));;

let egaux (a: 'a multiensemble) (b: 'a multiensemble) : bool =
    (inclus a b) && (inclus b a)
;;

(*Tests*)
assert (egaux cst_mens1 cst_mens1);;
assert (egaux cst_mens1 cst_mens3);;
assert_not (egaux cst_mens1 cst_mens2);;

let rec intersection (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    match a with
        | V -> V
        | A((value, occurences), rest) -> (
            let in_common_count = (min occurences (nbocc value b)) in
                if in_common_count = 0 then 
                (*Si il l'intersection est nulle pour cet élément*)
                    intersection rest b
                else
                    A((value, in_common_count), intersection rest b)
        )
;;

(*Tests*)
assert (cst_mens2 = intersection cst_mens1 cst_mens2);;
assert (A(('a', 3), V) = intersection cst_mens1 (A(('a', 4), V)));;

let rec difference (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    match b with
        | V -> a (* A - Ø = A *)
        | A(mel, rest) -> (difference (supprime mel a) rest)
;;

(*Tests*)
assert ((A(('a', 2), A(('b', 1), V))) = difference cst_mens1 cst_mens2);;

(*Retourne le n-ième du multi-ensemble *)
let rec ieme (n: int) (mens: 'a multiensemble) : 'a =
    if n < 0 then
        failwith "n < 0"
    else
        match mens with
            | V -> failwith "n >= cardinal mens"
            | A((value, occurences), rest) -> (
                if n < occurences then
                (*si c'est le n-ième élément, on le retourne*)
                    value
                else
                (*sinon on recommence pour l'élément suivant 
                en soustrayant les élément déja parcourus*)
                    ieme (n - occurences) rest
            )
;;

(*Tests*)
assert ('a' = ieme 1 cst_mens1);;
assert ('b' = ieme 4 cst_mens1);;
assert ('c' = ieme 5 cst_mens1);;

let un_dans (mens: 'a multiensemble) : 'a =
    let length = cardinal mens in
        ieme (Random.int length) mens
;;

(* Suite de test manuelle pour "un_dans" 
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
