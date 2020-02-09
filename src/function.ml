let id : 'a -> 'a = fun x -> x

let (%) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = fun f g x ->
  f (g x)

let (%>) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun f g x ->
  g (f x)

let const : 'a -> 'b -> 'a = fun x _ -> x

let eval : ('a -> 'b, 'a) Product.t -> 'b = fun (f, x) -> f x
