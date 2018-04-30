(* James Lindamood *)

(* TODO: 
1. Removed ;; 
2. made circle_circum_v1 one line
3. Broke the equation in circle_circum_v2 to the next line
4. There were no errors on running.
5. Added raised exception to sum_sqrdiffs for lists of size 1

Test: 	sum_sqrdiffs (1::[]);;
		Exception: Failure "sum_sqrdiffs input list needs at least two elements".

6. Added comment to triangle_parameter to clarify naming convention *)

let circle_circum_v1 r = 3.14159*.2.0*.r

let circle_circum_v2 r =
  let pi = 3.14159
  in 
  pi*.2.0*.r

let rec product listi =
  match listi with
  | [] -> 0
  | x::[] -> x
  | x::rest -> x * (product rest)

let rec sum_sqrdiffs listi =
  match listi with
  | [] -> 0
  | _::[] -> raise(Failure "sum_sqrdiffs input list needs at least two elements")
  | x1::(x2::[]) -> (x1-x2) * (x1-x2)
  | x1::(x2::rest) -> (x1-x2) * (x1-x2) + sum_sqrdiffs (x2::rest)

let distance (x1, y1) (x2, y2) = sqrt ( (y2-.y1)**2. +. (x2-.x1)**2. )


(* Tuples naming convention is in the form: i.e. point 1 x, point 1 y = (p1x, p2y) *)
let triangle_perimeter (p1x, p1y) (p2x, p2y) (p3x, p3y) = distance (p1x, p1y) (p2x, p2y) +.
                                                          distance (p2x, p2y) (p3x, p3y) +.
                                                          distance (p3x, p3y) (p1x, p1y)
