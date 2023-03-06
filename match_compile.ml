
module M =
  Matrix.Make(struct
      type entry = Pat.Typed.t
      type column = Occ.t
      type index = int
      let default_entry : entry = { v = Any; ty = Type.Prod [] }
      let default_column : column = { v = Base "unknown"; ty = Type.Prod [] }
      let default_index : index = (-1)
    end)

module OT = Hashtbl.Make(Occ)
module V = Vector

(* top-level only *)
let zonk : Pat.Typed.t -> Pat.Typed.t = fun p ->
  { p with ty = Type.compress p.ty }

(* preprocess list of patterns at a given occurrence into a pattern matrix *)
let preprocess (base : Occ.t) (ps : Pat.Typed.t list) res : M.t =
  let ps = List.map zonk ps in
  let seen = OT.create 10 in
  let header = ref [] in
  (* map occurrences to patterns, unpacks tuples into their components *)
  let occurrences_of (p : Pat.Typed.t) =
    let map = OT.create 10 in
    let add o p =
      OT.add map o p;
      if not (OT.mem seen o) then
        header := o :: !header;
      OT.add seen o ()
    in
    let go : Pat.Typed.t -> unit = function
      | { v = Tuple ps; ty = Prod ts } ->
         List.iteri (fun i p -> add { v = Proj (base, i); ty = List.nth ts i } p) ps
      | p -> add base p
    in
    go p;
    map
  in
  (* create and populate matrix *)
  let occs = List.map occurrences_of ps in
  let header = M.header_of_list (List.rev !header) in
  let matrix = M.create header in
  let add i map =
    let row = M.create_row () in
    let find o =
      let entry = match OT.find_opt map o with
        | Some p -> p
        | _ -> M.default_entry
      in
      V.push row entry
    in
    V.iter find header;
    M.add_row matrix row (res i)
  in
  List.iteri add occs;
  matrix

let show_pat =
  let rec go : Pat.Typed.t -> string = function
    | { v = Any; _ } -> "_"
    | { v = Var x; _ } -> x
    | { v = Cons (c, None); _ } -> c
    | { v = Cons (c, Some p); _ } ->
       Printf.sprintf "%s %s" c (go p)
    | { v = Tuple ps; _ } ->
       let ps = List.map go ps in
       Printf.sprintf "(%s)" String.(concat "," ps)
  in go

let is_refutable : Pat.Typed.t -> bool = function
  | { v = Cons _; _ } -> true
  | _ -> false (* NOTE: tuples can't appear at toplevel in matrix *)

let is_irrefutable =
  Fun.negate is_refutable

let vec_forall p v =
  let len = V.length v in
  let rec go = function
    | i when i >= len -> true
    | i -> if p (V.get v i) then go (i+1) else false
  in
  go 0

let has_refutable =
  Fun.negate (vec_forall is_irrefutable)

type dt = 
  | Fail
  | Leaf of int
  | Switch of Occ.t * (string * dt) list * dt option

let collect_signature ps =
  let ctors = Hashset.create 10 in
  let go : Pat.Typed.t -> _ = function
    | { v = Cons (c, _); _ } ->
       Hashset.add ctors c
    | _ -> ()
  in
  List.iter go ps;
  ctors

let unwrap st : Pat.Typed.t -> Pat.Typed.t = function
  | { v = Cons (_, Some p); _ } ->
     if Option.is_none !st then
       st := Some p.ty;
     p
  | { v = Cons (_, _); _ } ->
     M.default_entry
  | p -> p (* should this ever get hit? *)

let specialise (m : M.t) p =
  let patterns = ref [] in
  let len = V.length m.header in
  let ty = ref None in
  let go i row =
    match V.get row 0 with
    | pat when p pat ->
       let rem = V.sub row 1 (len - 1) in
       patterns := (unwrap ty pat, (M.get_index m i, rem)) :: !patterns
    | _ -> ()
  in
  V.iteri go m.rows;
  let pats, rest = List.(split (rev !patterns)) in
  let indices, remainders = Array.(split (of_list rest)) in
  let ty = Option.value ~default:(Type.Prod []) !ty in
  let occ : Occ.t = { v = Unwrap (M.get_header m 0); ty } in
  let m' = preprocess occ pats (Array.get indices) in
  V.(append m'.header (sub m.header 1 (len - 1)));
  V.iteri (fun i row -> V.append row remainders.(i)) m'.rows;
  m'

let admits c : Pat.Typed.t -> bool = function
  | { v = Cons (c', _); _ } -> c = c'
  | { v = (Var _ | Any); _ } -> true
  | _ -> false

let any : Pat.Typed.t -> bool = function
  | { v = (Var _ | Any); _ } -> true
  | _ -> false

let type_name t =
  Type.compress t
  |> (function
      | Type.Ctor (_, n) -> n
      | _ -> failwith "Not a valid type!")

let show_dt : Decision_tree.t -> string =
  let open Printf in
  let rec go t : Decision_tree.t -> string = function
    | { v = Fail; id } -> t ^ sprintf "fail [%d]\n" id
    | { v = Leaf i; id } -> t ^ sprintf "%d [%d]\n" i id
    | { v = Switch (o, cases, d); id } ->
       let cases = cases @ (match d with Some t -> [("default", t)] | _ -> []) in
       let cases =
         List.map (fun (tag, tree) -> Printf.sprintf "%s %s =>\n%s" t tag (go (t^"  ") tree)) cases
       in
       let cases = String.concat "" cases in
       let o = Occ.show o in
       t ^ sprintf "switch(%s) [%d] {\n%s%s}\n" o id cases t 
  in go ""

let compile arities base ps =
  let module DT = Decision_tree in
  let module DTB = DT.Make() in
  let initial = preprocess base ps Fun.id in
  let rec go (matrix : M.t) : DT.t =
    if M.is_empty matrix then
      DTB.get Fail
    else if vec_forall is_irrefutable (M.get_row matrix 0) then
      DTB.get (Leaf (M.get_index matrix 0))
    else
      begin
        let i = M.find_first_column matrix has_refutable in
        M.swap_columns matrix 0 i;
        let first = V.to_list (M.get_column matrix 0) in
        let signature = collect_signature first in
        let occ = M.get_header matrix 0 in
        let cases =
          Hashset.fold
            (fun c cs -> (c, go (specialise matrix (admits c))) :: cs) signature []
        in
        (* compute a default case if signature is inexhaustive *)
        let default =
          let tn = type_name occ.ty in
          if Hashset.cardinal signature < Hashtbl.find arities tn then
            Some (go (specialise matrix any))
          else
            None
        in
        DTB.get (Switch (occ, cases, default))
      end
  in
  go initial |> Graphviz.print |> print_endline

  
