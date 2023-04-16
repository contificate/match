
open Type

module Infer =
  Inference.Make(struct
      let next =
        let c = ref (-1) in
        fun () -> incr c; !c
    end)

let ps : Pat.Untyped.t list =
  let re : Pat.Untyped.t = Cons ("Red", None) in
  let bl : Pat.Untyped.t = Cons ("Black", None) in
  let t c l x r : Pat.Untyped.t =
    Cons ("T", Some(Tuple [c;l;x;r]))
  in
  [
    Tuple [bl; t re (t re Any Any Any) Any Any; Any; Any];
    Tuple [bl; t re Any Any (t re Any Any Any); Any; Any];
    Tuple [bl; Any; Any; t re (t re Any Any Any) Any Any];
    Tuple [bl; Any; Any; t re Any Any (t re Any Any Any)];
    Any
  ]

let env = {|
type 'a rbtree = 
  | E 
  | T of colour * 'a rbtree * 'a * 'a rbtree
and colour = 
  | Red 
  | Black
|}

let () =
  let input = Parser.program Lexer.tokenise (Lexing.from_string env) in
  let (ctors, arities) = Syntax.Type.reify_defs input in
  let ctx : Infer.ctx = { ctors } in
  let ty, ps' = Infer.infer_pats ctx ps in
  print_endline (Type.show ty);
  Match_compile.compile arities { v = Base "v"; ty } ps'

