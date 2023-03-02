
open Type
open Pat

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
  [Tuple [nil; Any];
   Tuple [Any; nil];
   Tuple [cons (Var "x") (Var "xs"); cons (Var "y") (Var "ys")]]

let () =
  let module T = Type in
  let ctors = Hashtbl.create 30 in
  let add = Hashtbl.add ctors in
  let q0 = T.QVar 0 in
  let q0l = T.Ctor ([q0], "list") in
  let q0o = T.Ctor ([q0], "option") in
  add "Nil" q0l;
  add "Cons" (T.Arrow (Prod [q0; q0l], q0l));
  add "None" q0o;
  add "Some" (T.Arrow (q0, q0o));
  let ctx : Infer.ctx =
    { ctors }
  in
  let ty, ps' = Infer.infer_pats ctx ps in
  print_endline (T.show ty)

