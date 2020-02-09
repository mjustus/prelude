module type S = sig
  include Functor.S

  val pure : 'a -> 'a t
  val lift : ('a -> 'b) t -> 'a t -> 'b t
end

module type OP = sig
  include Functor.OP

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *>) : 'a t -> 'b t -> 'b t
  val (<* ) : 'a t -> 'b t -> 'a t
end

module Op (A : S) : OP with type 'a t := 'a A.t = struct
  include Functor.Op(A)
  
  let (<$>) = A.map
  let (<*>) = A.lift
  let ( *>) a b = (fun _ y -> y) <$> a <*> b
  let (<* ) a b = (fun x _ -> x) <$> a <*> b
end
