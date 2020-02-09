module type TYPE = sig
  type t
end

module type TYPE0 = TYPE

module type TYPE1 = sig
  type 'a t
end

module type TYPE2 = sig
  type ('a, 'b) t
end

module type TYPE3 = sig
  type ('a, 'b, 'c) t
end

module String = struct
  type t = string
end

module Unit = struct
  type t = unit
end
