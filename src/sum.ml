type ('a, 'b) t =
  | Inl of 'a
  | Inr of 'b

let inl : 'a -> ('a, 'b) t = fun a -> Inl a
let inr : 'b -> ('a, 'b) t = fun b -> Inr b

let map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t = fun f g s ->
  match s with
  | Inl a -> Inl (f a)
  | Inr b -> Inr (g b)

let fold : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c = fun f g t ->
  match t with
  | Inl a -> f a
  | Inr b -> g b

let distribute_left : 'a 'b1 'b2. ('a, ('b1, 'b2) t) Product.t -> (('a, 'b1) Product.t, ('a, 'b2) Product.t) t = function
  | x, Inl y -> Inl (x, y)
  | x, Inr y -> Inr (x, y)

let distribute_right : 'a 'b1 'b2. (('b1, 'b2) t, 'a) Product.t -> (('b1, 'a) Product.t, ('b2, 'a) Product.t) t = function
  | Inl y, x -> Inl (y, x)
  | Inr y, x -> Inr (y, x)

let apply : ('a -> 'b, 'a -> 'c) t -> 'a -> ('b, 'c) t = fun t a ->
  match t with
  | Inl f -> inl @@ f a
  | Inr g -> inr @@ g a
