
type t =
  | Var of tv ref
  | QVar of int
  | Arrow of t * t
  | Prod of t list
  | Ctor of t list * string
and tv =
  | Unbound of int
  | Link of t

module Typed = struct
  type nonrec 'a t = {
      v: 'a;
      ty: t
    }
end
