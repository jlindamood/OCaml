open LazeeModules
open StreamModules

module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) : Hwk5Sig = struct
   (* add elements here to complete the functor *)

  type 'a stream = 'a S.t
  let take = S.take
  let head = S.head
  let zip = S.zip
  let filter = S.filter
  let map = S.map
  let delay = S.delay
  let demand = S.demand

  let rec from n = 
    print_endline ("step " ^ string_of_int n) ; 
    S.Cons ( n, 
         delay (fun () -> from (n+1) )
       )

  let nats = from 1

  let rec cubes_from i =
    S.Cons (i * i * i, delay (fun () -> cubes_from (i+1)))


  let rec cubes_from_zip i = zip ( * ) (zip ( * ) (from i) (from i)) (from i)

  let rec cubes_from_map i = map (fun x -> x*x*x) (from i)

  let rec drop n stream =
    match n, stream with
    | 0, _ -> stream
    | _, S.Cons (v, tl) -> drop (n-1) (demand tl)

  let rec drop_until f stream =
    match stream with
    | S.Cons (v, tl) -> if f v then stream
            else drop_until f (demand tl)

  let rec foldr (f: 'a -> 'b S.lazee -> 'b) (stream: 'a stream) : 'b =
  match stream with
  | S.Cons (v, tl) -> f v (delay (fun () -> (foldr f (demand tl))))


let rec and_fold stream =
  match stream with
  | S.Cons (v, tl) -> v && (and_fold (demand tl))

let pref x y = if x >= 0 then (x + demand (y)) else 0

let sum_positive_prefix stream = foldr pref stream

  let sift i stream =
      let not_is_multiple x y = not (y mod x = 0)
      in
      filter (not_is_multiple i) stream


  let rec sieve stream =
    match stream with
    | S.Cons (v, tl) -> S.Cons(v, delay (fun () -> sieve (sift v (demand tl))))

  let primes = sieve (from 2)
end