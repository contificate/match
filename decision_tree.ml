
type t = { v: t'; id: int }
and t' =
  | Fail
  | Leaf of int
  | Switch of Occ.t * (string * t) list * t option

let rec hash : t -> int = fun { v; _ } ->
  match v with
  | Fail | Leaf _ -> Hashtbl.hash v
  | Switch (o, cs, d) ->
     let cmp (s, _) (s', _) = compare s s' in
     let cs = List.sort cmp cs in
     Occ.hash o
     + List.fold_left (fun h (s, t) -> Hashtbl.hash s + hash t + h) 0 cs
     + Option.(value (map hash d) ~default:0)

let equal =
  let rec go t t' = match t, t' with
  | { id = i; _ }, { id = i'; _ } when i = i' -> true
  | { v = Fail; _ }, { v = Fail; _ } -> true
  | { v = Leaf i; _ }, { v = Leaf i'; _ } -> i = i'
  | { v = Switch (o, cs, d); _ }, { v = Switch (o', cs', d'); _ }
       when (Occ.equal o o') ->
     let cmp (s, _) (s', _) = compare s s' in
     let cs = List.sort cmp cs in
     let cs' = List.sort cmp cs' in
     List.equal (fun (s, t) (s', t') -> s = s' && go t t') cs cs'
     && Option.equal go d d'
  | _ -> false
  in go

module Make() = struct
  module TT =
    Hashtbl.Make(struct
        type nonrec t = t
        let hash = hash
        let equal = equal
      end)

  let cache : t TT.t = TT.create 100
  let c = ref (-1)

  let get t =
    let c' = !c + 1 in
    (match TT.find_opt cache { v = t; id = c'} with
     | Some tr -> tr
     | _ ->
        let entry = { v = t; id = c' } in
        TT.add cache entry entry;
        incr c; (* commit c' *)
        entry)
end
