module Make (E : Type.TYPE) (M : Monad.S) = struct
  module Error = E  
  type 'a t = ('a, Error.t) Sum.t M.t

  let pure : 'a. 'a -> 'a t = fun a ->
    M.pure (Sum.Inl a)
  let fail : 'a. E.t -> 'a t = fun e ->
    M.pure (Sum.Inr e)
  let bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun m f ->
    M.bind m (function
      | Sum.Inl a -> f a
      | Sum.Inr e -> fail e
    )

  let catch_map : 'a 'b. 'a t -> ('a -> 'b M.t) -> (E.t -> 'b M.t) -> 'b M.t = fun m f g ->
    M.bind m (function
      | Sum.Inl a -> f a
      | Sum.Inr e -> g e
    )

  let catch : 'a. 'a t -> (E.t -> 'a M.t) -> 'a M.t = fun m ->
    catch_map m M.pure
end
