module type S = sig
  type t

  val unit : t
  val mult : t -> t -> t
end

module Function (T : Type.TYPE) = struct
  type t = T.t -> T.t

  let unit = fun x -> x
  let mult = fun f g x -> f (g x)
end

module StringSet = struct
  include Set.Make (struct type t = string let compare = Stdlib.compare end)

  let unit = empty
  let mult = union
end

module Unit = struct
  type t = unit

  let unit = ()
  let mult () () = ()
end
