
open Decision_tree

let print (dt : t) : string =
  let open Printf in
  let open Decision_tree in
  let lines = ref [] in
  let append x = lines := x :: !lines in 
  let visited = Hashset.create 100 in
  let q = Queue.create () in
  Queue.push dt q;
  while not (Queue.is_empty q) do
    let { v; id } : t = Queue.pop q in
    if not (Hashset.mem visited id) then
      let label, shape, edges, children = match v with
        | Fail -> "fail", "Mdiamond", [], []
        | Leaf i -> string_of_int i, "doublecircle", [], []
        | Switch (o, cs, d) ->
           let cs = cs @ Option.(value (map (fun v -> [("default", v)]) d) ~default:[]) in
           let children, arcs = List.(split (map (fun (tag, ({ id = id'; _ } as v)) -> v, (id, id', tag)) cs)) in
           let arcs = List.map (fun (tag, { id = id'; _ }) -> (id, id', tag)) cs in
           Occ.show o, "egg", arcs, children
      in
      append (sprintf "%d [label=\"%s\", shape=\"%s\"];" id label shape);
      List.iter (fun (n, n', l) -> append (sprintf "%d -> %d [label=\"%s\"];" n n' l)) edges;
      List.iter (fun c -> Queue.push c q) children;
      Hashset.add visited id
  done;
  sprintf "digraph dt {\n%s\n}" (String.concat "\n" List.(rev !lines))
  
