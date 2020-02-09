module type S = sig
  module A : Applicative.S
  type 'a t

  val traverse : ('a -> 'b A.t) -> 'a t -> 'b t A.t
end

(* TODO move to Traversable2.S *)
module type S2 = sig
  module A : Applicative.S
  type ('a, 'b) t

  val traverse : ('a1 -> 'a2 A.t) -> ('b1 -> 'b2 A.t) -> ('a1, 'b1) t -> ('a2, 'b2) t A.t
end


module List (A : Applicative.S) : S with type 'a t = 'a list with module A = A = struct
  module A = A
  type 'a t = 'a list
  
  let rec traverse f xs =
    match xs with
    | []      -> A.pure []
    | x :: xs -> A.lift (A.map (fun x xs -> x :: xs)  (f x)) (traverse f xs)
end

module Option (A : Applicative.S) : S with type 'a t = 'a option with module A = A = struct
  module A = A
  type 'a t = 'a option

  let traverse f x =
    match x with
    | None    -> A.pure None
    | Some a  -> A.map (Option.some) (f a)
end

module N_list (A : Applicative.S) : S with type 'a t = 'a N_list.t with module A = A = struct
  module A = A

  type 'a t = 'a N_list.t
                         
  let rec traverse f x =
    let module Op = Applicative.Op (A) in
    let open Op in
    match x with
    | N_list.Last a      -> N_list.last <$> f a
    | N_list.More (a, x) -> N_list.more <$> f a <*> traverse f x

  module Applicative = A
end


(* TODO abstract *)
module Vec (A : Applicative.S) = struct
  module A = A
                         
  let rec traverse : type n. ('a -> 'b A.t) -> (n, 'a) Vec.t -> ((n, 'b) Vec.t) A.t = fun f x ->
    let module Op = Applicative.Op (A) in
    let open Op in
    match x with
    | Vec.Nil         -> A.pure Vec.Nil
    | Vec.Cons (a, x) -> Vec.cons <$> f a <*> traverse f x
end


(*
module Make = functor (F : Functor.FUNCTOR) (A : Applicative.S) ->
struct
  let rec sequence : 'a. 'a A.t F.t -> 'a F.t A.t = fun x ->
    traverse (fun x -> x) x
  and traverse : 'a 'b. ('a -> 'b A.t) -> 'a F.t -> 'b F.t A.t = fun f x ->
    sequence (F.map f x)
end
 *)
