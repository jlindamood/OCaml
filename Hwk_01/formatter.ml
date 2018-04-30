let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "
let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

(* Helper function from Eric Van Wyk *)
let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

let implode (cs: char list) : string =
	String.concat "" (List.map  (String.make 1) cs)

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

let format stringArg perLine =
	let exploded = explode stringArg
	in
	let words = split (fun x -> x = '\n' || x = ' ' || x = '\r' || x = '\t') exploded
	in
	let wordsAsString = List.map implode words
	in
	let wordsAsStringsTrimmed = List.filter (fun x -> x <> "") wordsAsString
	in
	let accum = ([], [], 0, 0)
	in
	(* a is an accumulator [lists of lines,[current]], a list of lists: each inner list is items to be printed in a line*)
	let f (lines, currentLine, currentListLength, currentListTotalStrLength) x =
		if perLine >= currentListTotalStrLength + currentListLength - 1 + String.length x
		then (lines, x::currentLine, currentListLength + 1, currentListTotalStrLength + String.length x)
		else (List.rev currentLine::lines, [x], 1, String.length x)
	in
	let (lines, curr, listLength, totalLength) = List.fold_left f accum wordsAsStringsTrimmed
	in
	let final = List.rev (List.rev curr::lines)
	in
	let finalTrimmed = List.filter (fun x -> x <> []) final
	in
	let formatted = List.map (String.concat " ") finalTrimmed
	in
	let finalFormatted = String.concat "\n" formatted
	in print_endline finalFormatted