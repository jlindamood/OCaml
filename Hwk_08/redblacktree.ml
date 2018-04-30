open Ordered

module type RedBlackSetSig = sig
    type elem
    type color = R | B
    type t = E | T of color * t * elem * t

    val empty: t
    val insert: elem -> t -> t
    val member: elem -> t -> bool
    val isRedBlackTree: t -> bool
  end

module RedBlackTree (S: OrderedSig): (RedBlackSetSig with type elem := S.t) = struct
    type elem = S.t
    type color = R | B
    type t = E | T of color * t * elem * t

    let empty = E

    let rec member x tree = match tree with
                        | E -> false
                        | T (_, a, y, b) ->
                            if S.lt x y then member x a
                            else if S.lt y x then member x b
                            else true

    let balance tree = match tree with
        | T (B, T (R, T (R, a, x, b), y, c), z, d) -> T (R, T (B,a,x,b), y, T (B,c,z,d))
        | T (B, T (R, a, x, T (R, b, y, c)), z, d) -> T (R, T (B,a,x,b), y, T (B,c,z,d))
        | T (B, a, x, T (R, T (R, b, y, c), z, d))  -> T (R, T (B,a,x,b), y, T (B,c,z,d))
        | T (B, a, x, T (R, b, y, T (R, c, z, d))) -> T (R, T (B,a,x,b), y, T (B,c,z,d))
        | _ -> tree

    let insert x s =
        let rec ins tree = match tree with
            | E -> T (R, E, x, E)
            | T (color, a, y, b) ->
                if S.lt x y then balance (T (color, ins a, y, b))
                else if S.lt y x then balance (T (color, a, y, ins b))
                else T(color, a, y, b)
            in
            let T (_, a, y, b) = ins s
        in T (B, a, y, b)

    let rec isRedBlackTree tree = 
      let getColor tree = match tree with
        | E -> B
        | T (color, _, _, _) -> color
      in match tree with
      | E -> true
      | T (color, t1, elem, t2) -> if color = R && ((getColor t1) = R || (getColor t2) = R) then false
                                else isRedBlackTree t1 && isRedBlackTree t2
    end

module RBTI = RedBlackTree (Int)