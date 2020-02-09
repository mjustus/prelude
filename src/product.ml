type ('a, 'b) t = 'a * 'b

let map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t = fun f g (a, b) ->
  (f a, g b)

let map1 : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t = fun f (a, b) ->
  (f a, b)

let map2 : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t = fun g (a, b) ->
  (a, g b)

let fold : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c = fun f (a, b) ->
  f a b

let pair : 'a -> 'b -> ('a, 'b) t = fun a b -> (a, b)

let diagonal : 'a -> ('a, 'a) t = fun x -> (x, x)

let curry : (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c = fun f a b -> f (a, b)
let uncurry :  ('a -> 'b -> 'c) -> (('a, 'b) t -> 'c) = fun f (a, b) -> f a b

let apply : ('a -> 'b, 'a -> 'c) t -> 'a -> ('b, 'c) t = fun (f, g) a -> (f a, g a)
