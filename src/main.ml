
open Type

module Infer =
  Inference.Make(struct
      let next =
        let c = ref (-1) in
        fun () -> incr c; !c
    end)

let ps : Pat.Untyped.t list =
  let nil : Pat.Untyped.t =
    Cons ("Nil", None)
  in
  let cons x xs : Pat.Untyped.t =
    Cons ("Cons", Some (Tuple [x; xs]))
  in
  let some x : Pat.Untyped.t =
    Cons ("Some", Some x) 
  in
  let a : Pat.Untyped.t = Cons ("A", None) in
  let b : Pat.Untyped.t = Cons ("B", None) in
  let c : Pat.Untyped.t = Cons ("C", None) in
  let re : Pat.Untyped.t = Cons ("Red", None) in
  let bl : Pat.Untyped.t = Cons ("Black", None) in
  let t c l x r : Pat.Untyped.t =
    Cons ("T", Some(Tuple [c;l;x;r]))
  in
  (* [
   *   Tuple [bl; t re (t re Any Any Any) Any Any; Any; Any];
   *   Tuple [bl; t re Any Any (t re Any Any Any); Any; Any];
   *   Tuple [bl; Any; Any; t re (t re Any Any Any) Any Any];
   *   Tuple [bl; Any; Any; t re Any Any (t re Any Any Any)];
   *   Any
   * ] *)
(* [Tuple [a; b; c]; Tuple [b;a;c]; Tuple [b; Any; b]] *)
(* [Tuple [nil; Any];
 *  Tuple [Any; nil];
 *  Tuple [cons (Var "x") (Var "xs"); cons (Var "y") (Var "ys")]] *)
  [some @@ Tuple [nil; Any];
   some @@ Tuple [Any; nil];
   some @@ Tuple [cons (Var "x") (Var "xs"); cons (Var "y") (Var "ys")];
   Cons ("None", None)]

let ctors, arities =
  let module T = Type in
  let ctors = Hashtbl.create 30 in
  let arities = Hashtbl.create 30 in
  let add n t =
    let get_ty : T.t -> string = function
      | T.Ctor (_, c) | T.Arrow (_, T.Ctor (_, c)) -> c
      | _ -> failwith "Not a valid constructor signature!"
    in
    Hashtbl.add ctors n t;
    let tn = get_ty t in
    (match Hashtbl.find_opt arities tn with
     | Some i -> Hashtbl.replace arities tn (i+1)
     | _ -> Hashtbl.add arities tn 1)
  in
  let q0 = T.QVar 0 in
  let q0l = T.Ctor ([q0], "list") in
  let q0o = T.Ctor ([q0], "option") in
  let bool = T.Ctor ([], "bool") in
  let colour = T.Ctor ([], "colour") in
  let abc = T.Ctor ([], "abc") in
  let q0t = T.Ctor ([q0], "rbtree") in
  add "Nil" q0l;
  add "Cons" (T.Arrow (Prod [q0; q0l], q0l));
  add "None" q0o;
  add "Some" (T.Arrow (q0, q0o));
  add "True" bool;
  add "False" bool;
  add "A" abc;
  add "B" abc;
  add "C" abc;
  add "Red" colour;
  add "Black" colour;
  add "E" q0t;
  add "T" (T.Arrow (Prod [colour; q0t; q0; q0t], q0t));
  ctors, arities

let () =
  let ctx : Infer.ctx = { ctors } in
  let ty, ps' = Infer.infer_pats ctx ps in
  print_endline (Type.show ty);
  Match_compile.compile arities { v = Base "v"; ty } ps'
