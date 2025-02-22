
open Js_of_ocaml

module Infer =
  Inference.Make(struct
      let next =
        let c = ref (-1) in
        fun () -> incr c; !c
    end)

let env = {|
type 'a rbtree = 
  | E 
  | T of colour * 'a rbtree * 'a * 'a rbtree
and colour = 
  | Red 
  | Black

match v with
| (Black, T (Red, T (Red, a, x, b), y, c), z, d)
| (Black, T (Red, a, x, T (Red, b, y, c)), z, d)
| (Black, a, x, T (Red, T (Red, b, y, c), z, d))
| (Black, a, x, T (Red, b, y, T (Red, c, z, d)))
| _
|}

let _default () =
  let (defs, occ, ps) = Parser.program Lexer.tokenise (Lexing.from_string env) in
  let (ctors, arities) = Syntax.Type.reify_defs defs in
  let ctx : Infer.ctx = { ctors } in
  let ty, ps' = Infer.infer_pats ctx ps in
  print_endline (Type.show ty);
  Match_compile.compile arities { v = Base occ; ty } ps'

let () =
  Js.export "match"
    (object%js
       method compile input =
         let src = Js.to_string input in
         let (defs, occ, ps) = Parser.program Lexer.tokenise (Lexing.from_string src) in
         let (ctors, arities) = Syntax.Type.reify_defs defs in
         let ctx : Infer.ctx = { ctors } in
         let ty, ps' = Infer.infer_pats ctx ps in
         let occ = Occ.{ v = Base occ; ty } in
         let dot = Match_compile.compile arities occ ps' in
         Js.string dot
     end)
