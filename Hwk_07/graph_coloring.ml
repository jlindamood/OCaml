(* By LINDA140, James Lindamood 
parts of this solution (and a large part of the structure, is entirely by or inspired by Eric Van Wyk*)


(* SHORT ANSWER SOLUTIONS *)

(*
	1. The search space my solution explores is at every step it evaluates whether or not all nodes have been accounted for. At that point,
	it goes thru all the nodes and makes sure that none of their adjacent nodes are of the same color.

	If one or both of these parameters are not met, the function recursively operates, branching into multiple search spaces: those assigning the next node the other colors.

	2. Part of my solution calls "isColoringValid and isValidByNode". These are written to be evaluated lazily, so that the second one node's adjacent neighbor has 
	the same color, it will exit and return false, at the beginning of each recursive call this is checked. This is made clear by its position as the first boolean value
	evaluated, so that lazy evaluation of nodesLeft = [], partial_subset <> = [] cause a lazy evaluation that results in isColoringValid not being called.

*)



(* Code below by Eric Van Wyk *)
type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

type 'a option = None
				| Some of 'a

let rec is_not_elem set v = not (List.mem v set)

let g1 = ( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )

let g1nodes = [N 1; N 2; N 3; N 4]

let g1edges = [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ]

let g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]

(* Searches a possible coloring and returns the color of a node *)
let rec colorOfNode n poss_coloring =
 	match poss_coloring with
 	| [] -> raise (Failure "Empty coloring")
 	| (n', c')::xs -> if n = n' then c' else colorOfNode n xs


(* Takes a list of Nodes and returns all possible colorings *)
let enumerateColorings nodelist =
	let rec enumerateColorings partial_subset rest =
		match rest with
		| [] -> [partial_subset]
		| x::xs -> (enumerateColorings ( ((x,C 1)::partial_subset) ) xs)
					@
					(enumerateColorings ( ((x,C 2)::partial_subset) ) xs)
					@
					(enumerateColorings ( ((x,C 3)::partial_subset) ) xs)
		in 
		enumerateColorings [] nodelist

(* Takes a node and graph and returns a list of nodes connected to that node *)
let rec connectedNodes (n: node) (edges: edge list) =
	let rec connectedHelper (accum: node list) (edges: edge list) =
		match edges with
		| [] -> accum
		| (n1, n2)::rest -> if n = n1 then connectedHelper ([n2] @ accum) rest
					else if n = n2 then connectedHelper ([n1] @ accum) rest
					else connectedHelper accum rest
	in connectedHelper [] edges


(* Given a node, checks if any adjacent nodes have the same color *)
let rec isValidByNode n edges poss_coloring =
	let adjnodes = connectedNodes n edges
	in
	let colorOfN = colorOfNode n poss_coloring
	in
	let rec helper nodesLeft =
		match nodesLeft with
		| [] -> true
		| x::xs -> if (colorOfNode x poss_coloring) = colorOfN then false
				else helper xs
	in helper adjnodes

let rec isColoringValid (nodes, edges) poss_coloring =
	match nodes with
	| [] -> true
	| n::ns -> isValidByNode n edges poss_coloring && isColoringValid (ns, edges) poss_coloring

let rec color_option ((nodes, edges): graph)  : (coloring option) =
	let rec helper coloringSoFar nodesLeft =
	if nodesLeft = [] && coloringSoFar <> [] && (isColoringValid (nodes, edges) coloringSoFar) 
	then Some coloringSoFar
else
	match nodesLeft with
	| [] -> None
	| x::xs -> match helper ( (x, C 1)::coloringSoFar ) xs with
			| Some result -> Some result
			| None -> match helper ( (x, C 2)::coloringSoFar) xs with
					| Some result -> Some result
					| None -> helper ( (x, C 3)::coloringSoFar) xs
	in helper [] nodes

exception FoundColoring of coloring

let rec color_exception ((nodes, edges): graph)  =
	let rec helper coloringSoFar nodesLeft =
	if nodesLeft = [] && coloringSoFar <> [] && (isColoringValid (nodes, edges) coloringSoFar) 
	then raise (FoundColoring coloringSoFar)
else
	match nodesLeft with
	| [] -> ()
	| x::xs -> 	helper ( (x, C 1)::coloringSoFar ) xs;
				helper ( (x, C 2)::coloringSoFar) xs;
				helper ( (x, C 3)::coloringSoFar) xs

	in helper [] nodes
	(*
	in try helper [] nodes; None with
		| FoundColoring result -> Some result *)