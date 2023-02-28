
open Type
open Pat

module Infer =
  Inference.Make(struct
      let next =
        let c = ref (-1) in
        fun () -> incr c; !c
    end)

let () =
  let ctx : Infer.ctx =
    { ctors = Hashtbl.create 30 }
  in
  ignore (Infer.infer ctx Any);
  print_endline "TODO"

