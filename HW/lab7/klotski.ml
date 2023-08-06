(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:                *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"


(* Part A *)
let bottom_square = LocSet.of_list[(4, 1); (4, 2); (3, 1); (3, 2)]
(* A.1 *)
let is_solved b =
  let cmp key value =
    LocSet.equal value bottom_square
  in
    CharMap.exists cmp b.pieces


(* A.2 *)
let compare b1 b2 =
  match LocSet.compare b1.unoccupied b2.unoccupied with
  | 0 -> 
    let (_, p1) = List.split (CharMap.bindings b1.pieces) in
    let (_, p2) = List.split (CharMap.bindings b2.pieces) in
    LocSetSet.compare (LocSetSet.of_list p1) 
                      (LocSetSet.of_list p2)
  | a -> a


(* A.3 *)
let remove c ({ pieces = p; unoccupied = u } as b) = 
  try let elem = CharMap.find c p in
    {pieces = CharMap.remove c p; unoccupied = LocSet.union elem u}
  with Not_found -> b


(* A.4 *)
let add (c, p) { pieces = ps; unoccupied = u } = 
  if not (CharMap.mem c ps) && LocSet.subset p u then
    Some { pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}
  else None


(* A.5 *)
let make_move (c, d, i) b =
  (* remove the piece from the board, and add it to the new location *)
  let remove_and_add c p b func =
    let board = remove c b in
    let ps = List.map func (LocSet.elements p) in
    add (c, LocSet.of_list ps) board in
  (* make a singular move depending on the direction *)
  let make_move_dir c p board = 
    match d with
      | Up -> remove_and_add c p board (fun (x, y) -> (x - 1, y))
      | Down -> remove_and_add c p board (fun (x, y) -> (x + 1, y))
      | Left -> remove_and_add c p board (fun (x, y) -> (x, y - 1))
      | Right -> remove_and_add c p board (fun (x, y) -> (x, y + 1))
  in
  (* if i is at least 1 and the piece exists on the board *)
  if i < 1 || not (CharMap.mem c b.pieces) then None
  else
    let rec iter step board = 
      let p = CharMap.find c board.pieces in
      if step = i then Some(board)
      else 
        match make_move_dir c p board with
        | Some v -> iter (step + 1) v
        | None -> None
    in
      iter 0 b 

 
(* A.6 *)
let next b =
  let test_move c p d b =
    let rec iter i res =
      match make_move (c, d, i) b  with
      | Some v -> iter (i + 1) (v :: res)
      | None -> res
    in
      iter 1 []
  in
  let test_move_all_dir c p b =
    List.concat_map (fun d -> test_move c p d b) [Left; Right; Up; Down] in
  List.concat_map (fun (c, p) -> test_move_all_dir c p b) 
                  (CharMap.bindings b.pieces)

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

