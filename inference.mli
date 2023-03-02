
module type Fresh = sig
  val next : unit -> int
end

module type S = sig
  type ctx = {
      ctors: (string, Type.t) Hashtbl.t
    }

  val infer : ctx -> Pat.Untyped.t -> Pat.Typed.t
  val infer_pats : ctx -> Pat.Untyped.t list -> Type.t * Pat.Typed.t list
end

module Make(F: Fresh): S

