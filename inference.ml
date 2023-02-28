
module type Fresh = sig
  val next : unit -> int
end

module type S = sig
  type ctx = {
      ctors: (string, Type.t) Hashtbl.t
    }

  val infer : ctx -> Pat.Untyped.t -> Pat.Typed.t
end

module Make(F: Fresh): S = struct
  open Type
  
  let fresh_tv () =
    Var (ref (Unbound (F.next ())))

  type ctx = {
      ctors: (string, t) Hashtbl.t
    }

  let inst t =
    let cache = Hashtbl.create 10 in
    let rec go = function
      | QVar q ->
         (match Hashtbl.find_opt cache q with
          | Some t' -> t'
          | _ ->
             let t' = fresh_tv () in
             Hashtbl.add cache q t';
             t')
      | Prod ts -> Prod List.(map go ts)
      | Arrow (l, r) -> Arrow (go l, go r)
      | Ctor (ts, c) -> Ctor (List.map go ts, c)
      | Var _ as t -> t
    in
    go t

  let unify t t' =
    if t == t' then () else
      match t, t' with
      | _ -> ()

  let infer (ctx : ctx) =
    let rec go : Pat.Untyped.t -> Pat.Typed.t = function
      | Any -> { v = Any; ty = fresh_tv () }
      | Var x -> { v = Var x; ty = fresh_tv () }
      | Tuple ps ->
         let ps_ty = List.map go ps in
         let get_ty : Pat.Typed.t -> _ =
           fun { ty; _ } -> ty
         in
         { v = Tuple ps_ty; ty = Prod List.(map get_ty ps_ty) }
      | Cons (c, po) ->
         let c_ty = inst (Hashtbl.find ctx.ctors c) in
         let po_ty = Option.map go po in
         let ty =
           (match po_ty with
            | Some p ->
               let cod = fresh_tv () in
               unify c_ty (Arrow (p.ty, cod));
               cod
            | _ -> c_ty)
         in
         { v = Cons (c, po_ty); ty }
    in go
  
end

