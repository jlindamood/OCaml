let rec ands (lst: bool list) : bool =
	match lst with
	| [] -> true
	| x::xs -> if x then ands xs
				else false  (* The second it encounters a false, it will exit *)