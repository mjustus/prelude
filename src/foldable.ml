module type S = sig
  module M : Monoid.S
  type 'a t

  val fold_map : ('a -> M.t) -> 'a t -> M.t
  val fold_mapi : (int -> 'a -> M.t) -> 'a t -> M.t
end
                                        
module List (M : Monoid.S) : S with type 'a t = 'a list with module M = M = struct
  module M = M
  type 'a t = 'a list
  
  let fold_map  f xs = List.fold_left M.mult M.unit (List.map  f xs)
  let fold_mapi f xs = List.fold_left M.mult M.unit (List.mapi f xs)
end
                                      
module Vec (N : Type.TYPE) (M : Monoid.S) : S with type 'a t = (N.t, 'a) Vec.t with module M = M = struct
  module M = M
  type 'a t = (N.t, 'a) Vec.t

  let fold_map  f xs = Vec.fold M.unit M.mult (Vec.map  f xs)
  let fold_mapi f xs = Vec.fold M.unit M.mult (Vec.mapi f xs)
end
