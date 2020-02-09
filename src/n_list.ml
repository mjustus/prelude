type 'a t =
  | Last of 'a
  | More of 'a * 'a t

let last : 'a -> 'a t = fun x -> Last x
let more : 'a -> 'a t -> 'a t = fun a x -> More (a, x)

let rec fold : ('a -> 'b) -> ('a -> 'b -> 'b) -> 'a t -> 'b = fun f g t ->
  match t with
  | Last a      -> f a
  | More (a, t) -> g a (fold f g t)

let rec map : ('a -> 'b) -> 'a t -> 'b t = fun f t ->
  match t with
  | Last a      -> Last (f a)
  | More (a, t) -> More (f a, map f t)
                     
let fold_map : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b = fun f m ->
  fold f (fun a -> m (f a))

let rec concat : 'a t -> 'a t -> 'a t = fun xs ys ->
  match xs with
  | Last x       -> More (x, ys)
  | More (x, xs) -> More (x, concat xs ys)
