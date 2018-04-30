module type OrderedSig = sig
    type t
    val eq: t -> t -> bool
    val lt: t -> t -> bool
    val leq: t -> t -> bool
  end

module Int : (OrderedSig with type t = int) = struct
    type t = int
    let eq x y = if x = y then true else false
    let lt x y = if x < y then true else false
    let leq x y = if x <= y then true else false
  end

module type BinomialHeapSig = sig
    type elem
    type tree = Node of int * elem * tree list
    type t = tree list
    val empty: tree
    val isEmpty: tree -> bool
    val insert: elem -> tree -> tree
    val merge: tree -> tree -> tree
    val findMin: tree -> elem
    val deleteMin: tree -> tree
  end

module BinomialHeap (S: OrderedSig) = struct
    type elem = S.t
    type tree = Node of int * elem * tree list
    type t = tree list
    let empty = []
    let isEmpty ts = if ts = [] then true else false
    let rank (Node (r,x,c)) = r
    let root (Node (r, x, c)) = x

    let link (Node (r1, x1, c1)) (Node (r2, x2, c2)) =
    	if S.leq x1 x2 then Node (r1+1, x1, (Node (r2, x2, c2))::c1)
    	else Node (r1+1, x2, (Node (r1, x1, c1))::c2)

    let rec insTree (t: tree) (ts: t) : t = match t, ts with
                      | t, [] -> [t]
                      | t, t'::ts' -> if rank t < rank t' then t::ts else insTree (link t t') ts'
    
    let insert (e: elem) ts = insTree (Node (0, e, [])) ts
    
    let rec merge ts1 ts2 = match ts1, ts2 with
                        | t1::ts1', [] -> ts1
                        | [], t2 :: ts2' -> ts2
                        | t1::ts1', t2 :: ts2' -> if rank t1 < rank t2 then t1::(merge ts1' ts2)
                          else if rank t2 < rank t1 then t2::(merge ts1 ts2')
                          else insTree (link t1 t2) (merge ts1' ts2')

    let rec removeMinTree ts = match ts with
                          | [] -> raise (Failure "Empty")
                          | [t] -> (t, [])
                          | t::ts -> let (t', ts') = removeMinTree ts in
                                      if S.leq (root t) (root t') then (t, ts) else (t', t::ts')
    
    let findMin ts = match removeMinTree ts with
                    | (t, _) -> root t
                    | _ -> raise (Failure "should not get here in findMin")
    
    let deleteMin ts = let (Node (_, x, ts1), ts2) = removeMinTree ts
    					in merge (List.rev ts1) ts2
end

module BHI = BinomialHeap(Int)

let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4

let m1 = BHI.findMin h5

let h6 = BHI.deleteMin h5

let m2 = BHI.findMin h6