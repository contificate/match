
module type P = sig
  type 'a t
end

module Spec(P: P) = struct
  type t = t' P.t
  and t' =
    | Any
    | Var of string
    | Tuple of t list
    | Cons of string * t option
end

module Identity = struct
  type 'a t = 'a
end

module Untyped = Spec(Identity)
module Typed = Spec(Type.Typed)
