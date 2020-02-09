open Function

module type S = sig
  type 'a t

  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module ToFunctor (M : S) : Functor.S with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t

  let map f m = M.bind m (fun x -> M.pure (f x))
end

module ToApplicative (M : S) : Applicative.S with type 'a t = 'a M.t = struct
  module F = ToFunctor (M)
  include F

  let pure = M.pure
  let lift m m' = M.bind m (fun f -> map f m')
end


module Operations (M : S) = struct
  let ignore : 'a M.t -> 'b M.t -> 'b M.t = fun m m' ->
    M.bind m (fun _ -> m')
end

module type OP = sig
  include Applicative.OP

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)  : 'a t -> 'b t -> 'b t
  val join  : 'a t t -> 'a t
  (* Kleisli composition. *)
  val (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
  val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
  val ( <**> ) : ('a -> 'b t) t -> 'a t -> 'b t
  val sum_distribute : ('a t, 'b t) Sum.t -> ('a, 'b) Sum.t t
end

module Op (M : S) : OP with type 'a t := 'a M.t = struct
  type 'a t = 'a M.t
  include Applicative.Op (ToApplicative (M))

  let (let*) = M.bind
  let (>>=) = M.bind
  let (>>) m m' =
    let module O = Operations (M) in
    O.ignore m m'
  let join m = m >>= Function.id
  let (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t) = fun f g ->
    join % (<$>) f % g
  let (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t) = fun f g ->
    g <=< f
  let (<**>) f m = join (f <*> m)

  let sum_distribute : ('a t, 'b t) Sum.t -> ('a, 'b) Sum.t t = function
    | Sum.Inl a -> Sum.inl <$> a
    | Sum.Inr b -> Sum.inr <$> b
end

module Id : S with type 'a t = 'a = struct
  type 'a t = 'a
  let pure x = x
  let bind x f = f x
end

module List : S with type 'a t = 'a list = struct
  type 'a t = 'a list

  let pure x = [x]
  let bind x f = List.concat (List.map f x)

end

module Lift = struct
  module Monoid (M : S) (S : Monoid.S) : Monoid.S with type t = S.t M.t = struct
    type t = S.t M.t

    let unit = M.pure S.unit
    let mult m m' =
      let module Op = Op (M) in
      let open   Op in
      S.mult <$> m <*> m'
  end
end
