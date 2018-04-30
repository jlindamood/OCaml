let circle_circum_v1 r =
  3.14159*.2.0*.r ;;

let circle_circum_v2 r =
  let pi = 3.14159
  in pi*.2.0*.r ;;

let rec product listi =
  match listi with
  | [] -> 0
  | x::[] -> x
  | x::rest -> x * (product rest) ;;

let rec sum_sqrdiffs listi =
  match listi with
  | [] -> 0
  | x1::[] -> 0
  | x1::(x2::[]) -> (x1-x2) * (x1-x2)
  | x1::(x2::rest) -> (x1-x2) * (x1-x2) + sum_sqrdiffs (x2::rest) ;;

let distance (x1, y1) (x2, y2) = sqrt ( (y2-.y1)**2. +. (x2-.x1)**2. );;

let triangle_perimeter (p1x, p1y) (p2x, p2y) (p3x, p3y) = distance (p1x, p1y) (p2x, p2y) +.
                                                          distance (p2x, p2y) (p3x, p3y) +.
                                                          distance (p3x, p3y) (p1x, p1y) ;;
