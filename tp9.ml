(* === Curryfication === *)

let add (a, b) = a + b
let () =
    assert (3 = add (3, 0));
    assert (8 = add (3, 5));
    assert (8 = add (5, 3))(*;
    assert (3 = add 3 0)*) (* Pas d'application partielle avec un tuple *)

let addC a b = a + b
let addC3 = addC 3

(*
  addC3 est une application partielle de addC avec 3.
  C'est un `int -> int`
  Si on l'applique avec un int `n`, on obtiendra la somme de `n` et 3

  (addC 3 5) et ((addC 3) 5) sont exactement les même expressions, il n'y a
  aucune différence. En OCaml, toutes les fonctions prennent exactement un
  argument, quand on execute (addC 3 5) on appelle en réalité deux fonctions à
  la suite.
*)

let rec addCbis (a: int): int -> int =
    if a = 0
    then function b -> b
    else function b -> 1 + (addCbis (a-1) b)

let () =
    let addC3 = addCbis 3 in
    assert (3 = addC3 0);
    assert (9 = addC3 6)


(* === Composition === *)

let comp g f x = g (f x)

let () =
    let identite = comp not not in
    assert (identite true);
    assert (not (identite false))

let incr n = n + 1
let plus_deux = comp incr incr

let () =
    assert (3 = plus_deux 1)

let fois_deux n = 2 * n
let fois_deux = ( * ) 2   (* Equivalent par application partielle de `*` *)

let f1 n = incr (fois_deux n)
let f1 = comp incr fois_deux

let f2 n = fois_deux (incr n)
let f2 = comp fois_deux incr

let () =
    assert (3 = f1 1);
    assert (4 = f2 1)


(* === Dérivation === *)

let dx = 0.001

let derivee f x = (f (x +. dx) -. f x) /. dx
let derivee2 = comp derivee derivee
let derivee3 = comp derivee derivee2

let () =
    Printf.printf "sin'(0) = %f\n" ((derivee sin) 0.);
    Printf.printf "sin''(0) = %f\n" ((derivee2 sin) 0.);
    Printf.printf "sin'''(0) = %f\n" ((derivee3 sin) 0.)

let rec iteration n =
    if n = 1
    then derivee
    else comp derivee (iteration (n - 1))

let () =
    let derivee3 = iteration 3 in
    Printf.printf "sin'''(0) = %f\n" ((derivee3 sin) 0.)


(* === Quantificateurs === *)

let non predicate x =
    not (predicate x)

let () =
    let pred = ((=) 1) in
    let non_pred = non pred in
    assert (non_pred 2);
    assert (not (non_pred 1))

let rec pour_tous predicate list =
    match list with
    | [] -> true
    | el :: rest -> predicate el && pour_tous predicate rest

let () =
    assert (pour_tous ((<>) 10) [1; 2; 3; 4; 5]);
    assert (not (pour_tous ((<>) 10) [1; 2; 10; 4; 5]))

let rec il_existe predicate list =
    match list with
    | [] -> false
    | el :: rest -> predicate el || il_existe predicate rest

let () =
    assert (il_existe ((=) 10) [1; 2; 3; 10; 5]);
    assert (not (il_existe ((=) 10) [1; 2; 3; 11; 5]))

let il_existe predicate = non (pour_tous (non predicate))

let () =
    assert (il_existe ((=) 10) [1; 2; 3; 10; 5]);
    assert (not (il_existe ((=) 10) [1; 2; 3; 11; 5]))


let l = [1015; 4305; 728; 861]
let multiple_de x n = (0 = n mod x)

let () =
    Printf.printf "l = [%s]\n" (String.concat "; " (List.map string_of_int l));
    Printf.printf "tous multiples de 7: %b\n" (List.for_all (multiple_de 7) l);
    Printf.printf "existe multiple de 13: %b" (List.exists (multiple_de 13) l)
