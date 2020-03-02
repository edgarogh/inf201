let int_of_bool b = if b then 1 else 0;;

let frac_to_point_in_square (i: int) (side: int) =
    assert (i < side * side);
    let x = i mod side
    and y = i / side
    in
        (
            (float_of_int x) /. (float_of_int side),
            (float_of_int y) /. (float_of_int side)
        )
;;

let in_circle ((x, y): float * float) = 1. >= (hypot x y);;

let pi n =
    let rec count_in_circle (i: int) (acc: int) =
        if i < (n*n) then
            let (x, y) = frac_to_point_in_square i n in
                (count_in_circle (i + 1) (acc + (int_of_bool (in_circle (x, y)))))
        else acc
    in
        4. *. float_of_int (count_in_circle 0 0) /. float_of_int (n*n)
;;

print_float (pi 5000);;
