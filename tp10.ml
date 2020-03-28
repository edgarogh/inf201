(* === Primalité et nombres de Mersenne === *)

let est_premier n =
    print_char '.'; (* Pour compter les appels *)
    let rec is_divisible x =
        if x*x <= n
        then (n mod x = 0) || is_divisible (x + 1)
        else false
    in
    not (is_divisible 2)

let () =
    assert (est_premier 7);
    assert (est_premier 23);
    assert (not (est_premier 100))

let rec puiss n =
    if n = 0
    then 1
    else 2 * (puiss (n - 1))

let () =
    assert (8 = puiss 3);
    assert (256 = puiss 8)

let mersenne p = (puiss p) - 1

let mersenne_rec =
    let rec find p =
        if est_premier p && est_premier (mersenne p)
        then ((puiss p) - 1)
        else find (p + 1)
    in
    find 2

let () = Printf.printf " recursif : %d\n" mersenne_rec

let rec creer_liste_entiers debut fin =
    if debut = fin
    then fin :: []
    else debut :: creer_liste_entiers (debut + 1) fin

let mersenne_liste =
    let entiers = creer_liste_entiers 2 20 in
    let premiers = List.filter est_premier entiers in
    let mersennes = List.map mersenne premiers in
    List.find est_premier mersennes

let () = Printf.printf " listes : %d\n" mersenne_liste


(* === Tri par insertion === *)

(* Utilitaire: afficher une liste (générique / ordre sup.) *)
let print_list to_string name l =
    let l = List.map to_string l in
    Printf.printf "%s = [%s]\n" name @@ String.concat "; " l

(* Utilitaire: afficher une liste d'int *)
let print_int_list = print_list string_of_int

let rec insertion_pos n new_el list =
    match list with
    | [] ->
        if n = 0
        then new_el :: []
        else failwith "n >= List.length list"
    | el :: rest ->
        if n = 0
        then new_el :: el :: rest
        else el :: insertion_pos (n - 1) new_el rest

let () =
    assert ([1; 2; 3] = insertion_pos 0 1 [2; 3]);
    assert ([1; 2; 3] = insertion_pos 1 2 [1; 3]);
    assert ([1; 2; 3] = insertion_pos 2 3 [1; 2])

let rec insertion cmp new_el list =
    match list with
    | [] -> new_el :: []
    | el :: rest ->
        if cmp el new_el >= 0
        then new_el :: el :: rest
        else el :: insertion cmp new_el rest

let () =
    assert ([1; 2; 3; 4; 5] = insertion (-) 1 [2; 3; 4; 5]);
    assert ([1; 2; 3; 4; 5] = insertion (-) 3 [1; 2; 4; 5]);
    assert ([1; 2; 3; 4; 5] = insertion (-) 5 [1; 2; 3; 4])

let tri_insertion cmp =
    List.fold_left (fun acc -> fun el -> insertion cmp el acc) []

let () =
    assert ([1; 2; 3; 4; 5] = tri_insertion (-) [3; 5; 2; 1; 4])

(* Tri: Entiers *)

let tri_croissant = tri_insertion (-)
let tri_decroissant = tri_insertion (fun a -> fun b -> b - a)

let () =
    let l = [2; 3; 12; 3; 24; 1; 2; 4; 9; 6; 10] in
    print_int_list "l_croissant" (tri_croissant l);
    print_int_list "l_decroissant" (tri_decroissant l)

(* Tri: Notes *)

type note = string * int
type releve = note list

let print_releve =
    print_list (fun (nom, note: note) -> Printf.sprintf "(%s, %d)" nom note)

let cmp_noms (nom_a, _: note) (nom_b, _: note) = compare nom_a nom_b
let cmp_notes (_, note_a: note) (_, note_b: note) = note_a - note_b

let tri_noms : releve -> releve = tri_insertion cmp_noms
let tri_notes : releve -> releve = tri_insertion cmp_notes

let () =
    (* Aucune inspi, j'avais une playlist Spotify devant moi *)
    let releve : releve = [
        "Niska",  59;
        "Naza",   53;
        "Maes",   41;
        "Ninho",  5;
        "PNL",    46;
        "Kaaris", 36
    ] in
    print_releve "releve_par_nom" (tri_noms releve);
    print_releve "releve_par_notes" (tri_notes releve)

(* Tri: Points *)

type point = float * float

let print_point_list =
    print_list (fun (x, y: point) -> Printf.sprintf "(%.0f, %.0f)" x y)

let dist0 (x, y) = hypot x y

let cmp_points = (fun a -> fun b -> compare (dist0 a) (dist0 b))
let tri_points = tri_insertion cmp_points

let () =
    let l = [(0., 1.); (1., 0.); (0., 0.); (5., -2.); (1., 1.)] in
    print_point_list "l_trie" (tri_points l)
