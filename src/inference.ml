
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

  let rec unify t t' =
    if t == t' then () else
      match t, t' with
      | Var ({ contents = Unbound _ } as tv), t'
        | t', Var ({ contents = Unbound _ } as tv) ->
         tv := Link t'
      | Var { contents = Link t }, t'
        | t', Var { contents = Link t } ->
         unify t t'
      | Prod ts, Prod ts' ->
         List.iter2 unify ts ts'
      | Ctor (ts, c), Ctor (ts', c') when c = c' ->
         List.iter2 unify ts ts'
      | Arrow (l, r), Arrow (l', r') ->
         unify l l';
         unify r r'
      | _ -> failwith "Unification error"

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

  let infer_pats (ctx : ctx) (ps : Pat.Untyped.t list) =
    let p_ty = fresh_tv () in
    let ps' = List.map (fun p -> let p' = infer ctx p in unify p_ty p'.ty; p') ps in
    p_ty, ps'

end

