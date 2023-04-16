
(** A syntactic type as parsed, not yet reified for inference. *)
module Type = struct
  type t =
    | Var of string
    | Ctor of t list * string
    | Prod of t list
    | Arrow of t * t

  type constr =
    | Unital of string
    | Valued of string * t

  type decl =
    | Variant of constr list

  (* type (...) c = ... and (...) c' = ... *)
  type defs = (string list * string * decl) list

  type reified = {
      codomain: Type.t;
      qvars: (string, Type.t) Hashtbl.t
    }

  (** transform parsed type definition as environment *)
  let reify_defs (defs : defs) =
    let module HT = Hashtbl in
    let types : (string, reified) HT.t = HT.create 10 in
    let collect_types (ps, t, _) =
      let qvars = HT.create 10 in
      let params =
        List.mapi
          (fun i p ->
            let qty = Type.QVar i in
            HT.add qvars p qty;
            qty) ps
      in
      HT.add types t { codomain = Type.Ctor (params, t); qvars }
    in
    List.iter collect_types defs;
    let generalise qvars =
      let rec go : t -> Type.t = function
        | Var x -> HT.find qvars x
        | Arrow (l, r) -> Arrow (go l, go r)
        | Prod ts -> Prod List.(map go ts)
        | Ctor (ps, c) -> Ctor (List.map go ps, c)
      in
      go
    in
    let ctors = HT.create 100 in
    let arities = HT.create 10 in
    let collect_ctors (_, t, Variant cs) =
      let { codomain; qvars } = HT.find types t in
      HT.add arities t List.(length cs);
      let go = function
        | Unital c -> HT.add ctors c codomain
        | Valued (c, typ) ->
           let domain = generalise qvars typ in
           HT.add ctors c (Arrow (domain, codomain))
      in
      List.iter go cs
    in
    List.iter collect_ctors defs;
    (ctors, arities)
end

