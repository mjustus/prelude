type 'a t = 'a option

let some : 'a -> 'a t = fun a -> Some a

let bind : 'a t -> ('a -> 'b t) -> 'b t = fun m f ->
  match m with
  | None   -> None
  | Some a -> f a

let map : ('a -> 'b) -> ('a t -> 'b t) = fun f m ->
  match m with
  | None   -> None
  | Some a -> Some (f a)

let lift : ('a -> 'b) t -> ('a t -> 'b t) = fun m m' ->
  bind m (fun f -> map f m')

let fail : 'a t = None
          
let pure : 'a -> 'a t = fun a -> Some a

module Operators = struct
  let (>>=) = bind
  let (<$>) = map
  let (<*>) = lift
end

let fold : ('a -> 'b) -> (unit -> 'b) -> 'a t -> 'b = fun f b ->
  function
  | Some a -> f a
  | None   -> b ()

let catch : 'a t -> (unit -> 'a) -> 'a = fun t f ->
  match t with
  | Some a -> a
  | None   -> f ()
