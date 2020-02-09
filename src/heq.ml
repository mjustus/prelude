module type S = sig
  include Type.TYPE1

  val hequal : 'a t -> 'b t -> bool
end

module Const (A : Eq.S) : S with type 'a t = A.t = struct
  type _ t = A.t

  let hequal = A.equal
end
