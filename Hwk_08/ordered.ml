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
