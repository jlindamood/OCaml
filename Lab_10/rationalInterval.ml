open Intervals

let compare_tuples (x,y) (x',y') =
	(* x/y ?= x'/y' ; *yy' = xy' = x'y *)
	let left_side = x * y' in
	let right_side = x' * y in
	if left_side < right_side then
		(-1)
	else if left_side = right_side then
		0
	else
		1

let rec euclid a b =
	if a = b then a
	else if a < b then euclid a (b-a)
	else euclid (a-b) b

let frac_simplify (a,b) =
	let gcd = euclid a b
	in
	(a/gcd, b/gcd)

let to_string_rationals (x, y) =
	let (x', y') = frac_simplify (x, y) in
	string_of_int x' ^ "/" ^ string_of_int y'

module Rational_comparable : (Comparable with type t = int * int) = struct
  type t = int * int
  let compare = compare_tuples
  let to_string = to_string_rationals
end

module Rational_interval = Make_interval(Rational_comparable)



(* The following line now works. *)
let i = Rational_interval.create (3, 7) (5, 3)
