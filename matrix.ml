
module type CellType = sig 
  type entry
  type column
  type index
  val default_entry : entry
  val default_column : column
  val default_index : index
end

module type S = sig
  
end

module Make(CT: CellType) = struct
  module V = Vector
  type entry = CT.entry
  type row = entry V.t
  type header = CT.column V.t
  type indices = CT.index V.t

  let dummy_row = V.create ~dummy:CT.default_entry
  let dummy_column = CT.default_column
  let dummy_index = CT.default_index

  let header_of_list cs =
    V.of_list ~dummy:dummy_column cs

  type t = {
      header: header;
      rows: row V.t;
      indices: indices;
    }

  let create_row () =
    V.create ~dummy:CT.default_entry

  let default_entry =
    CT.default_entry

  let create header : t = {
      header;
      rows = V.create ~dummy:dummy_row;
      indices = V.create ~dummy:dummy_index
    }

  let is_empty (m : t) =
    V.is_empty m.rows

  let add_row (m : t) row index =
    V.push m.rows row;
    V.push m.indices index

  let get_row (m : t) =
    V.get m.rows

  let get_index (m : t) =
    V.get m.indices

  let get_column (m : t) i =
    let column = create_row () in
    V.iter (fun r -> V.(push column (get r i))) m.rows;
    column

  let get_header (m : t) =
    V.get m.header

  let find_first_column (m : t) p =
    let len = V.length m.header in
    let rec go = function
      | i when i >= len -> failwith "No such column!" 
      | i -> if p (get_column m i) then i else go (i+1)
    in
    go 0

  let swap_columns (m : t) i i' =
    if i = i' then ()
    else
      let tmp = V.get m.header i in
      V.(set m.header i (get m.header i'));
      V.set m.header i' tmp;
      let swap row =
        let tmp = V.get row i in
        V.(set row i (get row i'));
        V.set row i' tmp
      in
      V.iter swap m.rows
end
