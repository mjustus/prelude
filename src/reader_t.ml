module type S = functor (R : Type.TYPE0) (M : Monad.S) -> sig
  include Type.TYPE1

  val cast : 'a M.t -> 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
           
  val ask : R.t t
  val local : (R.t -> R.t) -> 'a t -> 'a t
end
                                                       
module Make = functor (R : Type.TYPE) (M : Monad.S) -> struct
  type 'a t = R.t -> 'a M.t

  let cast : 'a. 'a M.t -> 'a t = fun m _ -> m
  let pure : 'a. 'a -> 'a t = fun a -> cast (M.pure a)
  let bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun m f r ->
    M.bind (m r) (fun a -> f a r)
           
  let ask : R.t t = fun x -> M.pure x
  let local : (R.t -> R.t) -> 'a t -> 'a t = fun f m r ->
    m (f r)
end

module Make1 = functor (R : Type.TYPE1) (M : Monad.S) -> struct
  type ('a, 'r) t = 'r R.t -> 'a M.t

  let cast : 'a 'r. 'a M.t -> ('a, 'r) t = fun m _ -> m
  let pure : 'a 'r. 'a -> ('a, 'r) t = fun a -> cast (M.pure a)
  let bind : 'a 'b 'r. ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t = fun m f r ->
    M.bind (m r) (fun a -> f a r)

  let map : 'a 'b 'r. ('a -> 'b) -> ('a, 'r) t -> ('b, 'r) t = fun f m ->
    bind m (fun x -> pure (f x))
  let lift : 'a 'b 'r. ('a -> 'b, 'r) t -> ('a, 'r) t -> ('b, 'r) t = fun m m' -> bind m (fun f -> map f m')
    
  let ask : 'r. ('r R.t, 'r) t = fun x -> M.pure x
  let local : 'r1 'r2. ('r2 R.t -> 'r1 R.t) -> ('a, 'r1) t -> ('a, 'r2) t = fun f m r ->
    m (f r)

  let join : 'a 'r. (('a, 'r) t, 'r) t -> ('a, 'r) t = fun m ->
    bind m Function.id

  module Op = struct
    let (let*) = bind
    let (>>=) = bind
    let (<$>) = map
    let (<*>) = lift
    let (>>)  = ignore
    let ( <**> ) : 'a 'b 'r. ('a -> ('b, 'r) t, 'r) t -> ('a, 'r) t -> ('b, 'r) t = fun f m ->
      join (f <*> m)
  end
end

module Make2 = functor (R : Type.TYPE2) (M : Monad.S) -> struct
  type ('a, 'r1, 'r2) t = ('r1, 'r2) R.t -> 'a M.t

  let cast : 'a 'r1 'r2. 'a M.t -> ('a, 'r1, 'r2) t = fun m _ -> m
  let pure : 'a 'r1 'r2. 'a -> ('a, 'r1, 'r2) t = fun a -> cast (M.pure a)
  let bind : 'a 'b 'r1 'r2. ('a, 'r1, 'r2) t -> ('a -> ('b, 'r1, 'r2) t) -> ('b, 'r1, 'r2) t = fun m f r ->
    M.bind (m r) (fun a -> f a r)

  let map : 'a 'b 'r1 'r2. ('a -> 'b) -> ('a, 'r1, 'r2) t -> ('b, 'r1, 'r2) t = fun f m ->
    bind m (fun x -> pure (f x))
  let lift : 'a 'b 'r1 'r2. ('a -> 'b, 'r1, 'r2) t -> ('a, 'r1, 'r2) t -> ('b, 'r1, 'r2) t = fun m m' -> bind m (fun f -> map f m')
    
  let join : 'a 'r1 'r2. (('a, 'r1, 'r2) t, 'r1, 'r2) t -> ('a, 'r1, 'r2) t = fun m ->
    bind m Function.id

  let ask : 'r1 'r2. (('r1, 'r2) R.t, 'r1, 'r2) t = fun x -> M.pure x
  let local : 'r1 'r2 's1 's2. (('s1, 's2) R.t -> ('r1, 'r2) R.t) -> ('a, 'r1, 'r2) t -> ('a, 's1, 's2) t = fun f m r ->
    m (f r)

  let ignore : 'a 'b 'r1 'r2. ('a, 'r1, 'r2) t -> ('b, 'r1, 'r2) t -> ('b, 'r1, 'r2) t = fun m m' ->
    bind m (fun _ -> m')
    
  module Op = struct
    let (let*) = bind
    let (>>=) = bind
    let (<$>) = map
    let (<*>) = lift
    let (>>)  = ignore
    let ( <**> ) : 'a 'b 'r1 'r2. ('a -> ('b, 'r1, 'r2) t, 'r1, 'r2) t -> ('a, 'r1, 'r2) t -> ('b, 'r1, 'r2) t = fun f m ->
      join (f <*> m)
  end
end
