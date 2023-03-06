
type t = { v: t'; ty: Type.t }
and t' =
  | Base of string
  | Proj of t * int
  | Unwrap of t

let hash = Hashtbl.hash

let rec equal o o' = match o, o' with
  | { v = Base x; _ }, { v = Base x'; _ } -> x = x'
  | { v = Proj (o, i); _ }, { v = Proj (o', i'); _ } when i = i' -> equal o o'
  | { v = Unwrap o; _ }, { v = Unwrap o'; _ } -> o = o'
  | _ -> false

let get_type : t -> Type.t =
  fun { ty; _ } -> ty
     
let rec show =
  let open Printf in
  function
  | { v = Base x; _ } -> x
  | { v = Proj (o, i); _ } ->
     let o = show o in
     sprintf "%s.%d" o i
  | { v = Unwrap o; _ } ->
     sprintf "[%s]" (show o)
