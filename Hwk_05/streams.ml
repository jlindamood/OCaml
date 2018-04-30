(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee

(* The above was given from the assignment *)
(* The below is taken from lazy.ml from the class repo *)

let rec from n = 
  print_endline ("step " ^ string_of_int n) ; 
  Cons ( n, 
         delay (fun () -> from (n+1) )
       )

let nats = from 1

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

 let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

(* The code below is from James Lindamood *)

let rec cubes_from i =
	Cons (i * i * i, delay (fun () -> cubes_from (i+1)))


let rec cubes_from_zip i = zip ( * ) (zip ( * ) (from i) (from i)) (from i)

let rec cubes_from_map i = map (fun x -> x*x*x) (from i)

let rec drop n stream =
	match n, stream with
	| 0, _ -> stream
	| _, Cons (v, tl) -> drop (n-1) (demand tl)

let rec drop_until f stream =
	match stream with
	| Cons (v, tl) -> if f v then stream
						else drop_until f (demand tl)

let rec foldr (f: 'a -> 'b lazee -> 'b) (stream: 'a stream) : 'b =
	match stream with
	| Cons (v, tl) -> f v (delay (fun () -> (foldr f (demand tl))))


let rec and_fold stream =
  match stream with
  | Cons (v, tl) -> v && (and_fold (demand tl))

let pref x y = if x >= 0 then (x + demand (y)) else 0

let sum_positive_prefix stream = foldr pref stream
	

let sift i stream =
		let not_is_multiple x y = not (y mod x = 0)
		in
	filter (not_is_multiple i) stream


let rec sieve stream =
	match stream with
	| Cons (v, tl) -> Cons(v, delay (fun () -> sieve (sift v (demand tl))))

let primes = sieve (from 2)