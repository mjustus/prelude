module type S = sig
  module Error : Type.TYPE       
  include Monad.S

  val fail : Error.t -> 'a t
  val catch_map : 'a t -> ('a -> 'b) -> (Error.t -> 'b) -> 'b
end

module Make (E : Type.TYPE) = Error_t.Make (E) (Monad.Id)

module String = Make (Type.String)
