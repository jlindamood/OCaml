let all_evens lst = List.filter (fun x -> if x mod 2 = 0 then true else false) lst

let increment_all lst = List.map (fun x -> x + 1) lst

let max_fold lst = List.fold_left (fun a x -> if a > x then a else x) (-9999999999) lst

let sum_prod lst = List.fold_left (fun (y,z) x -> (x+y, x*z)) (0, 1) lst


(* Rewritten from in class example of grp_by_3s *)
let split f lst = 
	let accum = ([],[])
	in
	let g (sublists, currentlist) x =
		if f x
		then (List.rev currentlist::sublists, [])
		else (sublists, x::currentlist)
	in 
	let (lsts, curr) = List.fold_left g accum lst
	in
	List.rev (List.rev curr::lsts)