
open Type
open Pat

module Infer =
  Inference.Make(struct
      let next =
        let c = ref (-1) in
        fun () -> incr c; !c
    end)

let p : Pat.Untyped.t =
  Cons ("Cons", Some (Tuple [Any; Cons ("Nil", None)]))

let () =
  let module T = Type in
  let ctors = Hashtbl.create 30 in
  let add = Hashtbl.add ctors in
  let q0 = T.QVar 0 in
  let q0l = T.Ctor ([q0], "list") in
  add "Nil" q0;
  add "Cons" (T.Arrow (Prod [q0; q0l], q0l));
  let ctx : Infer.ctx =
    { ctors }
  in
  let p' = Infer.infer ctx p in
  print_endline (T.show p'.ty)

