let read_file (file_name: string) : char list =
  let ic = open_in file_name 
  in 
  let rec read_chars ic =
    try 
      let next_char = input_char ic
      in next_char :: read_chars ic
    with 
      _ -> [] 
  in read_chars ic

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

let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"

let answers file_name =
	let words = split (fun x -> x = '\n' || x = ' ') (read_file file_name)
	in
	let wordsAsStrings = List.map implode words
	in
	let wordsAsStringsTrimmed = List.filter (fun x -> x <> "") wordsAsStrings
	in
	let wordsOfLength4 = List.filter (fun x -> String.length x = 4) wordsAsStringsTrimmed
	in
	let wordsOfLength6 = List.filter (fun x -> String.length x = 6) wordsAsStringsTrimmed
	in
	List.filter (fun x -> List.mem (String.sub x 1 4) wordsOfLength4) wordsOfLength6

let pretty_answers answersf =
	let accum = ([],[])
	in
	let f (a1,a2) x =
		(List.rev ((String.sub x 1 4)::a1), List.rev (x::a2))
	in
	List.fold_left f accum answersf

(* Python equivalent of what I want *)
(* for i in range wordsOfLength4
	for j in range wordsOfLength6
		if wordsOfLength4[i] == wordsOfLength6[j][1:4]:
			returnList.append wordsOfLength6[j] *)