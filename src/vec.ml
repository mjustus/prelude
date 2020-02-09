type (_, 'a) t =
  | Nil  : (T_nat.z, 'a) t
  | Cons : 'a * ('n, 'a) t -> ('n T_nat.s, 'a) t

let nil : 'a. (T_nat.z, 'a) t =
  Nil

let cons : type n. 'a -> (n, 'a) t -> (n T_nat.s, 'a) t = fun a t ->
  Cons (a, t)

let rec nth : type n. n Fin.t -> (n, 'a) t -> 'a = fun n t ->
  match n, t with
  | Fin.Z  , Cons (x, _) -> x
  | Fin.S n, Cons (_, t) -> nth n t

let rec ith : type n. int -> (n, 'a) t -> 'a option = fun i t ->
  match i, t with
  | 0, Cons (x, _) -> Some x
  | i, Cons (_, t) -> ith i t
  | _, Nil         -> None

let rec fold : type n. 'b -> ('a -> 'b -> 'b) -> (n, 'a) t -> 'b = fun f_n f_c t ->
  match t with
  | Nil         -> f_n
  | Cons (a, t) -> f_c a (fold f_n f_c t)

let rec map : type n. ('a -> 'b) -> (n, 'a) t -> (n, 'b) t = fun f t ->
  match t with
  | Nil         -> Nil
  | Cons (a, t) -> Cons (f a, map f t)

let mapi : type n. (int -> 'a -> 'b) -> (n, 'a) t -> (n, 'b) t = fun f t ->
  let rec mapi : type n. int -> (int -> 'a -> 'b) -> (n, 'a) t -> (n, 'b) t = fun i f t ->
    match t with
    | Nil         -> Nil
    | Cons (a, t) -> Cons (f i a, mapi (i+1) f t)
  in
  mapi 0 f t

let rec zip : type n. (n, 'a) t -> (n, 'b) t -> (n, 'a * 'b) t = fun t t' ->
  match t, t' with
  | Nil        , Nil          -> Nil
  | Cons (a, t), Cons (b, t') -> Cons ((a,b), zip t t')

let rec unzip : type n. (n, 'a * 'b) t -> (n, 'a) t * (n, 'b) t = fun t ->
  match t with
  | Nil         -> Nil, Nil
  | Cons (x, t) -> let v, v' = unzip t in
                   Cons (fst x, v), Cons (snd x, v')

let rec fold2 : type n. 'c -> ('a -> 'b -> 'c -> 'c) -> (n, 'a) t -> (n, 'b) t -> 'c = fun f_n f_c t t' ->
  match t, t' with
  | Nil        , Nil          -> f_n
  | Cons (a, t), Cons (b, t') -> f_c a b (fold2 f_n f_c t t')

let head : 'n. ('n T_nat.s, 'a) t -> 'a = function
  | Cons (a, _) -> a

let tail : 'n. ('n T_nat.s, 'a) t -> ('n, 'a) t = function
  | Cons (_, t) -> t

let length : type n. (n, 'a) t -> (module Type.TYPE) = fun _ ->
  let module Length = struct
    type t = n
  end in
  (module Length : Type.TYPE)
  

let rec to_list : type n. (n, 'a) t -> 'a list = fun xs ->
  match xs with
  | Nil          -> []
  | Cons (x, xs) -> x :: to_list xs

type 'a t_hidden =
  | Hidden : ('n, 'a) t -> 'a t_hidden

let rec from_list : 'a. 'a list -> 'a t_hidden = function
  | []      -> Hidden nil
  | x :: xs ->
     let (Hidden xs) = from_list xs in
     Hidden (cons x xs)
  
module Construct (X : Type.TYPE) = struct

  module type VALUE = sig
    val x : X.t
  end

  module type VECTOR = sig
    type n
    val xs : (n, X.t) t
  end

  module Nil : VECTOR = struct
    type n = T_nat.z
    let xs = Nil
  end

  let nil : (module VECTOR) =
    (module Nil : VECTOR)
  
  module Cons (X : VALUE) (XS : VECTOR) = struct
    type n = XS.n T_nat.s
    let xs = Cons (X.x, XS.xs)
  end

  let cons (x : X.t) (module XS : VECTOR) : (module VECTOR) =
    (module Cons (struct let x = x end) (XS) : VECTOR)
  
  let rec from_list (xs : X.t list) : (module VECTOR) =
    match xs with
    | []      -> nil
    | x :: xs -> cons x (from_list xs)

end
