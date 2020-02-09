type _ t =
  | Z : (_ T_nat.s) t
  | S : 'a t -> ('a T_nat.s) t

let z = 
  Z

let s : type n. n t -> (n T_nat.s) t = fun t ->
  S t
