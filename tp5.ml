let rec fact (n: int) : int =
    match n with
        |_ when n < 0 -> failwith "fact: n < 0"
        | 0 -> 1
        |_ -> n * fact (pred n)
;;

assert (6 = fact 3);;
assert (1 = fact 1);;

(* Q4 *)
(*
 On nous dit que la fonction n'est pas définie. En effet, `let` ne déclare une 
 valeur qu'après la fin de l'instruction (sans `rec`)
*)

(* Q5 *)
(*
 La fonction s'invoque récursivement à l'"infini" et continue dans les négatifs.
 On observe un dépassement de pile.
*)

let rec fact3 (n: int) =
    if n < 0 then failwith "factorielle: n < 0"
    else if n = 0 then 1
    else n * fact (pred n)
;;

assert (6 = fact3 3);;
assert (1 = fact3 1);;

(* TP5 -- Somme d'entiers bâton *)

type natPeano = Z | S of natPeano;;
type natBaton = Nil | Cons of char * natBaton;;

let rec addP (a: natPeano) (b: natPeano) : natPeano =
    match a with
        | Z -> b
        | S(p) -> addP p (S(b))
;;

assert ((S(S(S(Z)))) = addP (S(Z)) (S(S(Z))));;

let rec peanoVbaton (n: natPeano) : natBaton =
    match n with
        | Z -> Nil
        | S(n) -> Cons('|', peanoVbaton n)
;;

assert (Cons('|', Cons('|', Nil)) = peanoVbaton (S(S(Z))));;

let somme_baton (a: natPeano) (b: natPeano) =
    peanoVbaton (addP a b)
;;

assert (Cons('|', Cons('|', Nil)) = somme_baton Z (S(S(Z))));;
assert (Cons('|', Cons('|', Nil)) = somme_baton (S(Z)) (S(Z)));;
assert (Nil = somme_baton Z Z);;

let rec batonVpeano (n: natBaton) : natPeano =
    match n with
        | Nil -> Z
        | Cons(_, n) -> S(batonVpeano n)
;;

assert ((S(S(Z))) = batonVpeano (Cons('|', Cons('|', Nil))))

let somme_natbatons (a: natBaton) (b: natBaton) =
    somme_baton (batonVpeano a) (batonVpeano b)
;;


(* TP5 -- Quotient et reste de la division entière *)

let rec quotient (a: int) (b: int) =
    if a < 0 || b <= 0 then
        failwith "quotient: paramètres non supportés"
    else
        if a < b then
            0
        else
            1 + quotient (a - b) b
;;

assert (4 = quotient 16 4);;
assert (3 = quotient 10 3);;
assert (1 = quotient 5 5);;
assert (0 = quotient 2 3);;

(*

# quotient 17 5;;

    quotient <-- 17
    quotient --> <fun>
    quotient* <-- 5
        quotient <-- 12
        quotient --> <fun>
        quotient* <-- 5
            quotient <-- 7
            quotient --> <fun>
            quotient* <-- 5
                quotient <-- 2
                quotient --> <fun>
                quotient* <-- 5
                quotient* --> 0
            quotient* --> 1
        quotient* --> 2
    quotient* --> 3

*)
