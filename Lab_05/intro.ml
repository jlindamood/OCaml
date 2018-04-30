let even x = if x mod 2 = 0 then true else false

let rec euclid a b =
	if a = b then a
	else if a < b then euclid a (b-a)
	else euclid (a-b) b

let frac_simplify (a,b) =
	let gcd = euclid a b
	in
	(a/gcd, b/gcd)

let rec max lst =
	match lst with
	| [] -> raise (Failure "Input list must not be empty")
	| x::[] -> x
	| x::rest -> if x > max rest then x else max rest

let rec take a lst =
	if a < 1 then [] else
	match lst with
	| [] -> []
	| x::rest -> x::(take (a-1) rest)