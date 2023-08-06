type loc = int * int
type move = Up | Down | Left | Right

module type BoardRep =
  sig
    type t

    exception Invalid_move
    exception Invalid_location

    val init      : int -> t
    val load      : int -> int list -> t
    val get_size  : t -> int
    val get_hole  : t -> loc
    val get       : t -> loc -> int
    val make_move : t -> move -> t
    val show      : t -> unit
  end

(* ---------------------------------------------------------------------- 
 * Helper functions.
 * ---------------------------------------------------------------------- *)

(* Make a display function given board accessors. *)
let make_show get get_size b =
  let size = get_size b in
    begin
      Printf.printf "\n%!";
      for row = 0 to size - 1 do
        for col = 0 to size - 1 do
          Printf.printf "%3d" (get b (row, col))
        done;
        Printf.printf "\n";
      done;
      Printf.printf "\n%!"
    end

let rec find_idx_0 i lst =
  match lst with
  | [] -> failwith "Not Found"
  | h :: t -> if h = 0 then i else find_idx_0 (i + 1) t
(* ---------------------------------------------------------------------- 
 * Modules.
 * ---------------------------------------------------------------------- *)

module OrderedLoc =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module ArrayRep : BoardRep =
  struct
    type t = 
      { 
        acontents : int array;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size = 
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        {acontents = Array.init (size * size) 
                    (fun x -> (x + 1) mod (size * size)); 
         size = size; 
         hole = (size - 1, size - 1)}

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else if List.length lst <> size * size then
        failwith "ERROR: load: invalid list length"
      else if not (List.for_all (fun x -> List.mem x lst) 
              (List.init (size * size) (fun y -> y))) then 
              failwith "ERROR: load: invalid list contents"
      else
        let idx = find_idx_0 0 lst in
        {acontents = Array.of_list lst; 
         size = size; hole = (idx / size, idx mod size)}

    let get_size b = b.size

    let get_hole b = b.hole

    let get { acontents; size = s; _ } (r, c) = 
      if r < 0 || r > s - 1 || c < 0 || c > s - 1 then
        raise Invalid_location
      else
        try
          acontents.(r * s + c)
        with (Invalid_argument _) ->
          raise Invalid_location

    let make_move b m =
      let hole = get_hole b in 
      let size = get_size b in
      let loc_to_idx (x, y) =
        x * size + y in
      let swap h n contents =
        let n_idx = loc_to_idx n in
        contents.(loc_to_idx h) <- contents.(n_idx);
        contents.(n_idx) <- 0;
        contents
      in
      let new_board (x, y) = 
        if x < 0 || x >= size || y < 0 || y >= size then raise Invalid_move
        else {acontents = swap hole (x, y) (Array.copy b.acontents);
              size = size; hole = (x, y)} in
      match m, hole with
      | Up, (x, y) -> new_board (x - 1, y)
      | Down, (x, y) -> new_board (x + 1, y)
      | Left, (x, y) -> new_board (x, y - 1)
      | Right, (x, y) -> new_board (x, y + 1)

    let show = make_show get get_size
  end

module MapRep : BoardRep =
  struct
    module LocMap = Map.Make(OrderedLoc)

    type t = 
      { 
        mcontents : int LocMap.t;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let generate_map lst size =
      let rec iter row col lst res =
        match row, col with
        | _, b when b = size -> iter (row + 1) 0 lst res
        | a, _ when a = size -> res
        | _, _ -> iter row (col + 1) (List.tl lst)
                    (LocMap.add (row, col) (List.hd lst) res)
      in iter 0 0 lst LocMap.empty

    let init size =
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let list = List.init (size * size) 
                             (fun y -> (y + 1) mod (size * size)) in
        {mcontents = generate_map list size; 
          size = size; hole = (size - 1, size - 1)}

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else if List.length lst <> size * size then
        failwith "ERROR: load: invalid list length"
      else if not (List.for_all (fun x -> List.mem x lst) 
                  (List.init (size * size) (fun y -> y))) then 
              failwith "ERROR: load: invalid list contents"
      else 
        let idx = find_idx_0 0 lst in
        {mcontents = generate_map lst size; 
         size = size; hole = (idx / size, idx mod size)}

    let get_size b = b.size

    let get_hole b = b.hole

    let get { mcontents; _ } l = 
      try
        LocMap.find l mcontents
      with Not_found ->
        raise Invalid_location

    let make_move b m =
      let hole = get_hole b in 
      let size = get_size b in
      let swap h n contents =
        LocMap.add n 0 (LocMap.add h (LocMap.find n contents) contents)
      in
      let new_board (x, y) = 
        if x < 0 || x >= size || y < 0 || y >= size then raise Invalid_move
        else {mcontents = swap hole (x, y) b.mcontents;
              size = size; hole = (x, y)} in
      match m, hole with
      | Up, (x, y) -> new_board (x - 1, y)
      | Down, (x, y) -> new_board (x + 1, y)
      | Left, (x, y) -> new_board (x, y - 1)
      | Right, (x, y) -> new_board (x, y + 1)

    let show = make_show get get_size
  end

