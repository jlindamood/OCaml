(* By LINDA140, James Lindamood 
parts of this solution (and a large part of the structure, is entirely by or inspired by Eric Van Wyk*)


(*Explaining your solution

Write a comment near the top of your water_jug.ml file explaining how you represent the state of the problem. That is, what type do you use to represent the amount of water in the jugs?

You should define a type named state in your solution and use it for this purpose. Place this definition near the comment with the answer to this question.


ANSWER: I use a int * int tuple to represent the amount of water in the jugs. The first value is the four gallon jug, the second is the three gallon.

I do define a type named state, of type int * int.



*)

let rec is_not_elem set v = not (List.mem v set)

let describe (four:int) (three:int) : string = 
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

type state = int * int

let ok_state (fourg, threeg) : bool =
	0 <= fourg && fourg <= 4 && 0 <= threeg && threeg <= 3

let final s = (2, 0)

type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug

type 'a option = None
				| Some of 'a


let moves (st : state) : state list =
	let fill4gallonjugfromtap (fourg, threeg) =
		if fourg < 4 then [(4, threeg)] else [(100, 100)]
	in
	let fill3gallonjugfromtap (fourg, threeg) =
		if threeg < 3 then [(fourg, 3)] else [(100, 100)]
	in
	let empty4gallonjugonground (fourg, threeg) =
		if fourg > 0 then [(0, threeg)] else [(100, 100)]
	in
	let empty3gallonjugonground (fourg, threeg) =
		if threeg > 0 then [(fourg, 0)] else [(100, 100)]
	in
	let fill4gallonjugfrom3gallonjug (fourg, threeg) =
		if fourg < 4 && threeg > 0 && (fourg + threeg >= 4)
		then [(4, threeg - (4 - fourg))]
		else [(100, 100)]
	in
	let fill3gallonjugfrom4gallonjug (fourg, threeg) =
		if threeg < 3 && fourg > 0 && (fourg + threeg >= 3)
		then [( fourg - (3 - threeg), 3)]
		else [(100, 100)]
	in
	let empty4gallonjuginto3gallonjug (fourg, threeg) =
		if fourg > 0 && (fourg + threeg <= 3)
		then [(0, fourg + threeg)]
		else [(100, 100)]
	in
	let empty3gallonjuginto4gallonjug (fourg, threeg) =
		if threeg > 0 && (fourg + threeg <= 4)
		then [(fourg + threeg, 0)]
		else [(100, 100)]
	in 
	List.filter ok_state (fill4gallonjugfromtap st @ fill3gallonjugfromtap st
							@ empty4gallonjugonground st @ empty3gallonjugonground st
							@ fill4gallonjugfrom3gallonjug st @ fill3gallonjugfrom4gallonjug st
							@ empty4gallonjuginto3gallonjug st @ empty3gallonjuginto4gallonjug st)


exception FoundPath of (int * int) list

let rec decodepath path =
	let rec helper accum path =
		match path with
		| (fx, tx)::(fx',tx')::rest -> if (fx' <> fx) && (tx' = tx) && (fx' > fx) then helper ( [(Fill4GallonJugFromTap, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (fx' <> fx) && (tx' = tx) && (fx' < fx) then helper ( [(Empty4GallonJugOnGround, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (fx' = fx) && (tx' <> tx) && (tx' > tx) then helper ( [(Fill3GallonJugFromTap, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (fx' = fx) && (tx' <> tx) && (tx' < tx) then helper ( [(Empty3GallonJugOnGround, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (fx' > fx) && (tx' = 0) then helper ( [(Empty3GallonJugInto4GallonJug, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (fx' = 0) && (tx' > tx) then helper ( [(Empty4GallonJugInto3GallonJug, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (fx' = 4) then helper ( [(Fill4GallonJugFrom3GallonJug, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else if (tx' = 3) then helper ( [(Fill3GallonJugFrom4GallonJug, describe fx' tx')] @ accum) ((fx', tx')::rest)
							else raise (Failure "shouldn't get here")
		| _ -> List.rev (accum)
	in helper [] path

let play () =
	let rec go_from state path =
	if state = (2, 0)
	then raise (FoundPath path)
	else
	  let valid_moves : state list =  List.filter (is_not_elem path) (moves state)
	  in
	  let go_func m = go_from m (path @ [m])
	in 
	List.iter go_func valid_moves

in try go_from (0, 0) [(0,0)]; None
	with FoundPath path -> Some (decodepath path)