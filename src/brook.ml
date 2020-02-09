(* A brook is a stream. *)

type 'a t_data = {head : 'a; tail : 'a t}
and 'a t = 'a t_data Lazy.t

let cons : 'a -> 'a t -> 'a t_data = fun x t ->
  {head = x; tail = t}                                           
         
let head : 'a t -> 'a = fun t ->
  (Lazy.force t).head

let tail : 'a t -> 'a t = fun t ->
  (Lazy.force t).tail

let rec swap_data : ('a -> 'a -> bool) -> 'a -> 'a -> 'a t_data -> 'a t_data = fun equal x y t ->
  let z = t.head in
  if equal x z then
    cons y (swap equal x y t.tail)
  else if equal y z then
    cons x (swap equal x y t.tail)
  else
    cons z (swap equal x y t.tail)
and swap : ('a -> 'a -> bool) -> 'a -> 'a -> 'a t -> 'a t = fun equal x y t ->
  lazy (swap_data equal x y (Lazy.force t))
  
let rec prepend : 'a list -> 'a t -> 'a t = fun xs t ->
  match xs with
   | []      -> t
   | x :: xs -> lazy (cons x (prepend xs t))

let rec prepend_data : 'a list -> 'a t_data -> 'a t_data = fun xs t ->
  match xs with
   | []      -> t
   | x :: xs -> cons x (lazy (prepend_data xs t))

let rec prepend_tail : 'a list -> 'a t -> 'a t_data = fun xs t ->
  match xs with
   | []      -> Lazy.force t
   | x :: xs -> cons x (lazy (prepend_tail xs t))
              
let rec map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t = fun f t ->
  lazy (map_data f (Lazy.force t))
and map_data : 'a 'b. ('a -> 'b) -> 'a t_data -> 'b t_data = fun f t ->
  cons (f t.head) (map f t.tail)
  
let rec filter_data : ('a -> bool) -> 'a t_data -> 'a t_data = fun p t ->
  if p t.head then
    cons t.head (filter p t.tail)
  else
    filter_data p (Lazy.force t.tail)
and filter : ('a -> bool) -> 'a t -> 'a t = fun p t ->
  lazy (filter_data p (Lazy.force t))
