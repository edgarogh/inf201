type point = float * float;;

type figure 
    = Triangle of point * point * point
    | Cercle of point * float
    | Rectangle of point * point;;

let dist ((x1, y1): point) ((x2, y2): point) =
    hypot (x1 -. x2) (y1 -. y2)
;;

assert (2. = dist (0., 0.) (2., 0.));;
assert (5. = dist (1., 0.) (4., 4.));;

let pi = acos (-1.);;

let peri (f: figure) : float =
    match f with
        | Triangle(p1, p2, p3) -> dist p1 p2 +. dist p2 p3 +. dist p1 p3
        | Cercle(c, r) -> 2. *. pi *. r
        | Rectangle((x1, y1), (x2, y2)) -> 2. *. (abs_float (x1 -. x2) +. abs_float (y1 -. y2))
;;

let c = Cercle((1., 2.), 1.);;
print_float (peri c);;
