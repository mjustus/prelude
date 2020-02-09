module Make (S : Type.TYPE) (M : Monad.S) = struct
  type 'a t = S.t -> ('a * S.t) M.t

  let pure : 'a -> 'a t = fun a s ->
    M.pure (a, s)
  let pure_m : 'a M.t -> 'a t = fun m s ->
    M.bind m (fun a -> M.pure (a, s))
  let bind : 'a t -> ('a -> 'b t) -> 'b t = fun m f s ->
    M.bind (m s) (fun (a, s) -> f a s)
  let run : 'a t -> S.t -> 'a M.t = fun m s ->
    M.bind (m s) (fun (a, _) -> M.pure a)
  let run_state : 'a t -> S.t -> ('a * S.t) M.t = fun m s ->
    M.bind (m s) M.pure
  let read : S.t t = fun s ->
    M.pure (s, s)
  let write : (S.t -> S.t) -> unit t = fun f s ->
    M.pure ((), f s)
  let read_write : (S.t -> S.t) -> S.t t = fun f s ->
    M.pure (s, f s)
  let bind_m : 'a t -> ('a -> 'b M.t) -> 'b t = fun m f ->
    bind m (fun a s -> M.bind (f a) (fun a -> M.pure (a, s)))
  let map_m : ('a -> 'b) M.t -> 'a t -> 'b t = fun f m ->
    bind m (fun a s -> M.bind f (fun f -> M.pure (f a, s)))
  let scoped : 'a t -> 'a t = fun m ->
    bind read                 (fun s ->
    bind m                    (fun b ->
    bind (write (fun _ -> s)) (fun _ ->
    pure b)))
  let lift : 'a M.t -> 'a t = fun m ->
    let module F = Monad.ToFunctor (M) in
    fun s -> F.map (fun a -> a, s) m
end
