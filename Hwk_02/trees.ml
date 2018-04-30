(* Part A *)

exception Invalid_input;;

type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree

type 'a option = None
              | Some of 'a

let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))

let t5 : string option tree =
  Fork (Some "a",
        Leaf (Some "b"),
        Fork (Some "c",
              Leaf None,
              Leaf (Some "d")))

let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

let rec t_size tree =
  match tree with
  | Leaf _ -> 1
  | Fork (_, y, z) -> 1 + t_size y + t_size z

let rec t_sum tree =
  match tree with
  | Leaf x -> x
  | Fork(x, y, z) -> x + t_sum y + t_sum z

let rec t_charcount (tree: string tree) : int =
  match tree with
  | Leaf x -> String.length x
  | Fork (x, y, z) -> String.length x + t_charcount y + t_charcount z

let rec t_concat (tree: string tree) : string =
  match tree with
  | Leaf x -> x
  | Fork (x, y, z) -> x ^ t_concat y ^ t_concat z

(* Part B *)

let rec t_opt_size optree =
  match optree with
  | Leaf None -> 0
  | Leaf Some _ -> 1
  | Fork (x, y, z) -> 1 + t_opt_size y + t_opt_size z

let rec t_opt_sum optree =
  match optree with
  | Leaf None -> 0
  | Leaf Some x -> x
  | Fork (Some x, y, z) -> x + t_opt_sum y + t_opt_sum z
  | Fork (None, _, _) -> raise Invalid_input

let rec t_opt_charcount optree =
  match optree with
  | Leaf None -> 0
  | Leaf Some x -> String.length x
  | Fork (Some x, y, z) -> String.length x + t_opt_charcount y + t_opt_charcount z
  | Fork (None, _, _) -> raise Invalid_input

let rec t_opt_concat (optree: string option tree) =
  match optree with
  | Leaf None -> ""
  | Leaf Some x -> x
  | Fork (Some x, y, z) -> x ^ t_opt_concat y ^ t_opt_concat z
  | Fork (None, _, _) -> raise Invalid_input

(* Part C *)

let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b = 
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size tree =
	let g x =
		1
	in
	let f a b c =
		1 + b + c
	in
	tfold g f tree

let tf_sum tree =
	let g x =
		x
	in
	let f a b c =
		a + b + c
	in
	tfold g f tree

let tf_charcount tree =
	let g x =
		String.length x
	in
	let f a b c =
		String.length a + b + c
	in
	tfold g f tree

let tf_concat tree =
	let g x =
		x
	in
	let f a b c =
		a ^ b ^ c
	in
	tfold g f tree

let tf_opt_size tree =
	let g x =
		match x with
		| None -> 0
  		| Some _ -> 1
	in
	let f a b c =
		g a + b + c
	in
	tfold g f tree

let tf_opt_sum tree =
	let g x =
		match x with
		| None -> 0
  		| Some v -> v
	in
	let f a b c =
		g a + b + c
	in
	tfold g f tree

let tf_opt_charcount tree =
	let g x =
		match x with
		| None -> 0
  		| Some v -> String.length v
	in
	let f a b c =
		g a + b + c
	in
	tfold g f tree

let tf_opt_concat tree =
	let g x =
		match x with
		| None -> ""
  		| Some v -> v
	in
	let f a b c =
		g a ^ b ^ c
	in
	tfold g f tree

type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree

let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

let rec bt_insert_by c e tree =
	let newNode = Node(Empty, e, Empty)
	in
	match tree with
	| Empty -> newNode
	| Node(Empty, v, Empty) -> if c v e = 1 || c v e = 0 then Node(newNode, v, Empty)
								else Node(Empty, v, newNode)
	| Node (l, v, r) -> if c v e = 1 || c v e = 0 then Node(bt_insert_by c e l, v, r)
						else Node(l, v, bt_insert_by c e r)

let rec bt_elem_by c e tree =
	match tree with
	| Empty -> false
	| Node(Empty, v, Empty) -> c v e
	| Node (l, v, r) -> if c v e = true then true
						else bt_elem_by c e l || bt_elem_by c e r

let rec bt_to_list tree =
	match tree with
	| Empty -> []
	| Node(Empty, v, Empty) -> [v]
	| Node (l, v, r) -> (bt_to_list l) @ [v] @ (bt_to_list r)

let rec btfold base f tree =
	match tree with
	| Empty -> base
	| Node (l, v, r) -> f (btfold base f l) v (btfold base f r)

let btf_elem_by c e tree =
	let f l v r =
		l || (c v e) || r
	in
	btfold false f tree

let btf_to_list tree =
	let f l v r =
		l @ [v] @ r
	in
	btfold [] f tree

(*
	btf_insert_by: Why would creating this be difficult?

	btf_insert_by would have to be implemented with a treeAccumulate version
	of tree fold which presents issues with branching. The function
	passed into btfold takes 3 parameters. Let's say we wanted to insert by < the value of 4
	in the tree below:

	We'd look at the root node, match on node(l, v, r) and call f on the result of the 
	left tree, the value 5, and the right tree. But to build up the answer to the left and right tree, you have to hit the base case
	and recurse all the way to the Empty case in every sub tree.

	In the case of insert_by in our recursive implementation, we operate in n lg n time (lg n to place and n to build)

	but in insert_by with fold, we'd operate in n^2 time because we have to look at every node to place the value, since f can only return a value when the depth
	of each subtree has been reached and the base case reached multiple times. Whereas in our current implementation, we reach the base case only once, and
	build up the tree from there.


	
	   5
  	  / \
 	 3   7
    /\	/ \
   1 ()() ()
  / \
 () ()
*)