(*
  Caltech WI 2022-23
  CS 4 Lab 6
  Rachael Kim
*)

(* Part A *)
(* A.1 *)
(* let factorial n =
  let rec iter m r =
    if m = 0
      then r
      else iter (m - 1) (r * m)
  in iter n 1
in
  factorial 3 *)

(*
  -- Desugar this expression to:
  let factorial = fun n ->
    let rec iter = fun m r ->
      if m = 0 then r
      else iter (m - 1) (r * m)
    in iter n 1
  in
    factorial 3

  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]
      = : [primitive function =]

  FUNCTION 0 (fun n -> ...)
    env: FRAME 0
    param: n
    body: let rec iter = fun m r -> ...
    
  FRAME 1 (let factorial = FUNCTION 0 in ...)
    parent: FRAME 0
    bindings:
      factorial: FUNCTION 0

  FRAME 2 (FUNCTION 0 applied to 3)
    parent: FRAME 0
    bindings:
      n : 3
  
  FRAME 3 (let rec iter = FUNCTION 1 in ...)
    parent: FRAME 2
    bindings:
      iter: FUNCTION 1

  FUNCTION 1 (fun m r -> ...)
    env: FRAME 3
    param: m, r
    body: if m = 0 then r
          else iter (m - 1) (r * m)

  FRAME 4 (FUNCTION 1 applied to n and 1)
    parent: FRAME 3
    bindings:
      m : 3
      r : 1

  FRAME 5 (FUNCTION 1 applied to 2, 3)
    parent: FRAME 3
    bindings:
      m : 2
      r : 3

  FRAME 6 (FUNCTION 1 applied to 1, 6)
    parent: FRAME 3
    bindings:
      m : 1
      r : 6

  FRAME 7 (FUNCTION 1 applied to 0, 6)
    parent: FRAME 3
    bindings:
      m : 0
      r : 6
*)


(* A.2 *)
let factorial =
  let f = ref (fun _ -> 0) in
  begin
    f := (fun n -> if n = 0 then 1 else n * !f (n - 1));
    !f;
  end


(* B.1 *)
exception Stat_error of string

let make_stat_1 () = 
  let sum = ref 0.0 in
  let sumsq = ref 0.0 in
  let n = ref 0 in
  object
    method append v = 
      sum := !sum +. v;
      sumsq := !sumsq +. v *. v;
      n := !n + 1;
    method mean = 
      if !n = 0 then raise (Stat_error "need at least one value for mean")
      else !sum /. float_of_int(!n)
    method variance =
      if !n = 0 then raise (Stat_error "need at least one value for variance")
      else !sumsq /. float_of_int(!n) -. 
        (!sum /. float_of_int(!n)) *. (!sum /. float_of_int(!n))
    method stdev =
      if !n = 0 then raise (Stat_error "need at least one value for stdev")
      else sqrt(!sumsq /. float_of_int(!n) -. 
        (!sum /. float_of_int(!n)) *. (!sum /. float_of_int(!n)))
    method clear =
      sum := 0.0;
      sumsq := 0.0;
      n := 0;
  end

  let make_stat_2 () = 
    let sum = ref 0.0 in
    let sumsq = ref 0.0 in
    let n = ref 0 in
    object (self)
      method append v = 
        sum := !sum +. v;
        sumsq := !sumsq +. v *. v;
        n := !n + 1;
      method mean = 
        if !n = 0 then raise (Stat_error "need at least one value for mean")
        else !sum /. float_of_int(!n)
      method private _variance = !sumsq /. float_of_int(!n) -. 
        (!sum /. float_of_int(!n)) *. (!sum /. float_of_int(!n))
      method variance =
        if !n = 0 then raise (Stat_error "need at least one value for variance")
        else self#_variance
      method stdev =
        if !n = 0 then raise (Stat_error "need at least one value for stdev")
        else sqrt(self#_variance)
      method clear =
        sum := 0.0;
        sumsq := 0.0;
        n := 0;
    end
  
  
(* Part C *)
(* C.1 *)
(* Signature for priority queues. *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty
    type elem = int
    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t
    let empty = Leaf
    let is_empty t =
      match t with
      | Leaf -> true
      | Node (_, _, _, _) -> false
    let rec merge t1 t2 = 
      match t1, t2 with
      | Leaf, Leaf -> Leaf
      | Leaf, _ -> t2
      | _, Leaf -> t1
      | Node (rk1, v1, l1, r1), Node (rk2, v2, l2, r2) ->
        if v1 <= v2 then
          Node (min rk1 rk2 + 1, v1, l1, merge r1 t2)
        else 
          Node (min rk1 rk2 + 1, v2, l2, merge r2 t1)
    let insert t i = merge (Node (1, i, Leaf, Leaf)) t
    let find_min t =
      match t with
      | Leaf -> raise Empty
      | Node (_, v, _, _) -> v
    let delete_min t =
      match t with
      | Leaf -> raise Empty
      | Node (_, _, l, r) -> merge l r
    let from_list lst =
      let rec iter res node =
        match res with
        | [] -> node
        | h :: t -> iter t (insert node h)
      in
        iter lst Leaf
  end

let heap_sort lst =
  let rec iter res t =
    match PriorityQueue.is_empty t with
    | true -> List.rev res
    | false -> iter (PriorityQueue.find_min t :: res) (PriorityQueue.delete_min t) 
  in
    iter [] (PriorityQueue.from_list lst)


(* C.2 *)
(* Signature for ordered objects. *)
module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end

module MakePriorityQueue (Elt : ORDERED_TYPE)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty
    type elem = Elt.t
    type t = Leaf | Node of int * elem * t * t
    let empty = Leaf
    let is_empty t =
      match t with
      | Leaf -> true
      | Node (_, _, _, _) -> false
    let rec merge t1 t2 = 
      match t1, t2 with
      | Leaf, Leaf -> Leaf
      | Leaf, _ -> t2
      | _, Leaf -> t1
      | Node (rk1, v1, l1, r1), Node (rk2, v2, l2, r2) ->
        if Elt.compare v1 v2 = -1 then
          Node (min rk1 rk2 + 1, v1, l1, merge r1 t2)
        else 
          Node (min rk1 rk2 + 1, v2, l2, merge r2 t1)
    let insert t i = merge (Node (1, i, Leaf, Leaf)) t
    let find_min t =
      match t with
      | Leaf -> raise Empty
      | Node (_, v, _, _) -> v
    let delete_min t =
      match t with
      | Leaf -> raise Empty
      | Node (_, _, l, r) -> merge l r
    let from_list lst =
      let rec iter res node =
        match res with
        | [] -> node
        | h :: t -> iter t (insert node h)
      in
        iter lst Leaf
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
  let rec iter res t =
    match StringPQ.is_empty t with
    | true -> List.rev res
    | false -> iter (StringPQ.find_min t :: res) (StringPQ.delete_min t) 
  in
    iter [] (StringPQ.from_list lst)


(* Part D *)
(* D.1 *)
type 'a contents = Unevaluated of (unit -> 'a) | Evaluated of 'a
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Unevaluated e)
let force lz =
  match !lz with
  | Unevaluated e -> 
    let v = e () in
    (lz := Evaluated v;
     v)
  | Evaluated e -> e


(* D.2 *)
(* D.2.a *)
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

let almost_sum =
  fun f ->
    fun lst ->
      match lst with
      | [] -> 0
      | h :: t -> h + f t

let sum = y almost_sum


(* D.2.b *)
let factorial n =
  let rec iter n r =
    if n = 0
      then r
      else iter (n - 1) (n * r)
  in
    iter n 1

let factorial2 n =
  let almost_iter =
    fun f ->
      fun (n, r) ->
        if n = 0
          then r
          else f (n - 1, n * r)
in
  (y almost_iter) (n, 1)
