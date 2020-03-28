type 'a ab = Nil | Node of 'a * 'a ab * 'a ab
type ab_int = int ab

(* Utilitaire: Afficher un arbre *)
let print_tree to_string root_ab =
    let term_yellow s = "\x1b[33;1m" ^ s ^ "\x1b[0m" in
    let rec print_tree prefix prefix_b ab =
        match ab with
        | Nil -> print_endline (prefix ^ "Nil")
        | Node (value, sub1, sub2) ->
            print_endline (prefix ^ term_yellow @@ to_string value);
            if (sub1 <> Nil || sub2 <> Nil)
            then (
                print_tree (prefix_b ^ "├─") (prefix ^ "│ ") sub1;
                print_tree (prefix_b ^ "└─") (prefix ^ "  ") sub2
            )
            else ()
    in
    print_tree "" "" root_ab

let ab1 : ab_int =
    Node (15, Node (7, Nil, Node (13, Nil, Nil)), Node (33, Nil, Nil))

let ab2 : ab_int =
    Node (15, Node (7, Node (13, Nil, Nil), Nil), Node (33, Nil, Nil))

let () =
    print_endline "ab1 =";
    print_tree string_of_int ab1

let () =
    print_endline "ab2 =";
    print_tree string_of_int ab2

let abS n : ab_int = Node (n, Nil, Nil)
let abUNd (n, ab: int * ab_int) : ab_int = Node(n, Nil, ab)
let abUNg (ab, n: ab_int * int) : ab_int = Node(n, ab, Nil)

let ab1_f : ab_int = Node (15, abUNd (7, abS 13), abS 33)
let ab2_f : ab_int = Node (15, abUNg (abS 13, 7), abS 33)

let () =
    assert (ab1 = ab1_f);
    assert (ab2 = ab2_f)


(* === Somme === *)

let rec somme ab =
    match ab with
    | Nil -> 0
    | Node (value, sub1, sub2) ->
        value
        + somme sub1
        + somme sub2

let () =
    assert (68 = somme ab1);
    assert (68 = somme ab2)


(* === Hauteur === *)

let hauteur ab =
    let rec hauteur depth ab =
        match ab with
        | Nil -> 0
        | Node (_, sub1, sub2) ->
            1 + max (hauteur depth sub1) (hauteur depth sub2)
    in
    hauteur 0 ab

let () =
    assert (0 = hauteur Nil);
    assert (1 = hauteur (abS 42));
    assert (3 = hauteur ab1)


(* === Symétrique === *)

(*
  SPECIFICATION:
  (sym ab) renvoie le symétrique de l'arbre (ab) dans un mirroir.
  ALGO:
  On procède récursivement en inversant les branches.
  TERMINAISON:
  La fonction est appellée récursivement autant de fois qu'il y a de `Node` ou
  de `Nil` dans l'arbre.
*)
let rec sym ab =
    match ab with
    | Nil -> Nil
    | Node (value, sub1, sub2) -> Node (value, sym sub2, sym sub1)

let () =
    print_endline "ab1_sym =";
    print_tree string_of_int (sym ab1)

(*
  SPECIFICATION:
  (sontSym ab1 ab2) renvoie true si (ab1) est le symétrique de (ab2).
  ALGO:
  On renvoie une application partielle de l'opérateur d'égalité avec comme
  paramètre le symmétrique d'un arbre passé en paramètre. Cela renvoie une
  fonction de type `'a ab -> bool` à laquelle on peut passer le second arbre.
  TERMINAISON:
  La fonction n'est pas directement récursive (elle dépend cependant de `sym`).
*)
let sontSym ab1 = (=) (sym ab1)

let () =
    assert (sontSym ab1 (sym ab1))
