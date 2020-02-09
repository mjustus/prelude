module type S = sig
  include Type.TYPE1

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type OP = sig
  type 'a t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module Op (A : S) : OP with type 'a t := 'a A.t = struct
  let (let+) t f = A.map f t
end

module List = struct
  type 'a t = 'a list
  let map = List.map

  include Op(List)
end

module N_list = struct
  type 'a t = 'a N_list.t
  let map = N_list.map

  include Op(N_list)
end
