
module type Fresh = sig
  val next : unit -> int
end

module type S = sig
  type ctx = {
      ctors: (string, Type.t) Hashtbl.t
    }

  val infer : ctx -> Pat.Untyped.t -> Pat.Typed.t
end

module Make(F: Fresh): S

