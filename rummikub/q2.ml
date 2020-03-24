(* Specification: assertion inverse *)
let assert_not cond = assert (not cond);;

(*Types*)
type 'a multielement = 'a * int;;
type 'a multiensemble = 'a multielement list;;

(*Variables pour test*)
let cst_me1 : char multielement = ('a', 3);;
let cst_me2 : char multielement = ('b', 2);;
let cst_me3 : char multielement = ('c', 1);;

let cst_me4 : char multielement = ('a', 1);;
let cst_me5 : char multielement = ('b', 1);;

let cst_me6 : char multielement = ('a', 1);;
let cst_me7 : char multielement = ('b', 3);;
let cst_me8 : char multielement = ('c', 2);;

let cst_mens1 : char multiensemble = [cst_me1; cst_me2; cst_me3];;
let cst_mens2 : char multiensemble = [cst_me4; cst_me5; cst_me3];;
let cst_mens3 : char multiensemble = [cst_me6; cst_me7; cst_me8];;

let rec cardinal (mens: 'a multiensemble) : int =
    match mens with
    | [] -> 0
    | (_, occurences) :: rest -> occurences + cardinal rest
;;

(*Tests*)
assert (0 = cardinal []);;
assert (3 = cardinal cst_mens2);;
assert (6 = cardinal cst_mens1);;

let rec nbocc (which: 'a) (mens: 'a multiensemble) : int =
    match mens with
    | [] -> 0
    | (el, occurences) :: rest -> (
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
    | [] -> true (* L'ensemble vide est dans tous les ensembles *)
    | (el, occurences) :: rest -> (
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
        | [] -> []
        | (value, occurences) :: rest when (n_value = value) -> (
                let mel_sum : 'a multielement = (value, occurences + n_occ) in
                if (snd mel_sum) <= 0 then (* Peut se produire lors d'une suppression *)
                    rest
                else
                    mel_sum :: rest
            )
        | mel :: rest -> let a = ajoute (n_value, n_occ) rest in
                mel :: a
    else (* Elément pas présent, on le rajoute au début tel quel *)
        (n_value, n_occ) :: mens
;;


assert (cst_mens1 = (ajoute ('a', 2) (ajoute ('b', 1) cst_mens2)));;
assert ([('a', 1); ('b', 1)] = (ajoute ('c', -1) cst_mens2));;

let supprime ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
    ajoute (n_value, -n_occ) mens
;;

assert ([('a', 1); ('b', 1)] = (supprime ('c', 1) cst_mens2));;
assert ([('a', 3); ('c', 1)] = (supprime ('b', 4) cst_mens1));;

let egaux (a: 'a multiensemble) (b: 'a multiensemble) : bool =
    (inclus a b) && (inclus b a) (* Double inclusion *)
;;

assert (egaux cst_mens1 cst_mens1);;
assert_not (egaux cst_mens1 cst_mens3);;
assert_not (egaux cst_mens1 cst_mens2);;

let rec intersection (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    match a with
    | [] -> []
    | (value, occurences) :: rest -> (
            let in_common_count = (min occurences (nbocc value b)) in
            if in_common_count = 0 then
                intersection rest b
            else
                (value, in_common_count) :: (intersection rest b)
        )
;;

assert (cst_mens2 = intersection cst_mens1 cst_mens2);;
assert ([('a', 3)] = intersection cst_mens1 ([('a', 4)]));;

let rec difference (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    match b with
    | [] -> a (* A - Ø = A *)
    | mel :: rest -> (difference (supprime mel a) rest)
;;

assert (([('a', 2); ('b', 1)]) = difference cst_mens1 cst_mens2);;

let rec ieme (n: int) (mens: 'a multiensemble) : 'a =
    if n < 0 then
        failwith "n < 0"
    else
        match mens with
        | [] -> failwith "n >= cardinal mens"
        | (value, occurences) :: rest -> (
                if n < occurences then
                    value
                else
                    ieme (n - occurences) rest
            )
;;

assert ('a' = ieme 1 cst_mens1);;
assert ('b' = ieme 4 cst_mens1);;
assert ('c' = ieme 5 cst_mens1);;

let un_dans (mens: 'a multiensemble) : 'a =
    let length = cardinal mens in
    ieme (Random.int length) mens
;;

(* Suite de test manuelle pour "un_dans" *)

let ens = [('a', 2); ('b', 1)];;

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
