module type S = sig
  type t

  val equal : t -> t -> bool
end

let equal (type t) (module E : S with type t = t) : t -> t -> bool =
  E.equal

module String = struct
  type t = string

  let equal : t -> t -> bool =
    fun x y -> Stdlib.compare x y = 0
end
