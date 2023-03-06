
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

let rec compress = function
  | Var ({ contents = Link t } as tv) ->
     let t' = compress t in
     tv := Link t;
     t'
  | Arrow (l, r) ->
     Arrow (compress l, compress r)
  | Ctor (ts, c) -> Ctor (List.map compress ts, c)
  | Prod ts -> Prod List.(map compress ts)
  | Var _ | QVar _ as t -> t

let rec show =
  let open Printf in
  let go = function
    | Var { contents = Link t } ->
       show t
    | Var { contents = Unbound i } ->
       sprintf "'t%d" i
    | Ctor ([], c) -> c
    | Ctor ([t], c) -> sprintf "(%s) %s" (show t) c
    | Ctor (ts, c) ->
       let ts = String.concat ", " List.(map show ts) in
       sprintf "(%s) %s" ts c
    | Prod [] -> "unit"
    | Prod [t] -> show t
    | Prod ts ->
       String.concat " * " List.(map show ts)
    | _ -> failwith "irrelevant"
  in fun t -> go (compress t)
