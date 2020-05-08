(* Specification: assertion inverse *)
let assert_not cond = assert (not cond);;

(* Specification: composition (existe déjà par défaut techniquement) *)
let (@@) f x = f x;;

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


(* Fonction auxiliaire *)
(*
  Doc:
    Prend un multiensemble et renvoye un multiensemble "propre", c.a.d que les
    multielements nuls ou negatifs ont été filtrés.
*)
let nettoyer (mens: 'a multiensemble) : 'a multiensemble =
    List.filter (fun ((_, occ): 'a multielement) -> occ > 0) mens
;;

(**
  Algo:
    On mappe chaque multielement vers son nombre d'occurences, cela donne une
    list d'int, puis on fait la somme de cette liste.
*)
let cardinal (mens: 'a multiensemble) : int =
    List.fold_left (+) 0 (List.map snd mens)
;;

(*Tests*)
assert (0 = cardinal []);;
assert (3 = cardinal cst_mens2);;
assert (6 = cardinal cst_mens1);;

(**
  Algo:
    On filtre pour ne garder que l'unique multielement de la même valeur que
    which et on retourne le cardinal de ce multiensemble.
*)
let nbocc (which: 'a) (mens: 'a multiensemble) : int =
    cardinal (List.filter (fun mel -> which = fst mel) mens)
;;

(*Tests*)
assert (3 = nbocc 'a' cst_mens1);;
assert (1 = nbocc 'c' cst_mens1);;

let appartient (which: 'a) (mens: 'a multiensemble) : bool =
    (nbocc which mens) > 0
;;

(*Tests*)
assert (appartient 'b' cst_mens1);;
assert_not (appartient 'd' cst_mens1);;

let inclus (mens1: 'a multiensemble) (mens2: 'a multiensemble) : bool =
    if mens2 = [] then
        mens1 = []
    else
    List.for_all (fun (n_value, n_occ) ->
        nbocc n_value mens1 <= n_occ
    ) mens2
;;

(*Tests*)
assert_not (inclus cst_mens1 []);;
assert (inclus [] []);;
assert (inclus cst_mens2 cst_mens1);;
assert_not (inclus cst_mens1 cst_mens2);;

(**
  Algo:
    Si le multiensemble contient la valeur du multielement, on utilise map pour
    remplacer le multielement qui matche par lui même avec le nombre
    d'occurences modifié.
    Sinon, on rajoute le nouveau multielement au début.
 *)
let ajoute ((n_value, n_occ): 'a multielement) (mens: 'a multiensemble) : 'a multiensemble =
    nettoyer @@
    if appartient n_value mens then
        List.map (fun (c_value, c_occ) ->
            let new_occ = if c_value = n_value then c_occ + n_occ else c_occ in
            (c_value, new_occ)
        ) mens
    else
        (n_value, n_occ) :: mens
;;

(*Tests*)
assert (cst_mens1 = (ajoute ('a', 2) (ajoute ('b', 1) cst_mens2)));;
assert ([('a', 1); ('b', 1)] = (ajoute ('c', -1) cst_mens2));;

let supprime ((n_value, n_occ): 'a multielement) = ajoute (n_value, -n_occ);;

assert ([('a', 1); ('b', 1)] = (supprime ('c', 1) cst_mens2));;
assert ([('a', 3); ('c', 1)] = (supprime ('b', 4) cst_mens1));;

ignore (nettoyer [(1, 0)]);;

let intersection (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    nettoyer @@
    List.map (fun (el, a_occ) -> (
        let b_occ = nbocc el b in
        (el, min a_occ b_occ)
    )) a
;;

assert (cst_mens2 = intersection cst_mens1 cst_mens2);;
assert ([('a', 3)] = intersection cst_mens1 [('a', 4)]);;

let difference (a: 'a multiensemble) (b: 'a multiensemble) : 'a multiensemble =
    let supprime a b = supprime b a in (* Inversion des paramètres *)
    List.fold_left supprime a b
;;

assert (([('a', 2); ('b', 1)]) = difference cst_mens1 cst_mens2);;
