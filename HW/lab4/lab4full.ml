(*
  Caltech WI 2022-23
  CS 4 Lab 4
  Rachael Kim
*)


(* Part A *)
(* A.1 SICP 2.2 *)
type point = {x:float; y:float}
type segment = {startp:point; endp:point}

let make_point x y = {x=x; y=y}
let get_coords {x; y} = (x,y)
let make_segment pt1 pt2 = {startp = pt1; endp = pt2}
let get_points {startp; endp} = startp, endp
let get_x {x; y} = x
let get_y {x; y} = y

let midpoint_segment {startp; endp} = 
  make_point ((startp.x +. endp.x) /. 2.0) ((startp.y +. endp.y) /. 2.0)

let segment_length {startp; endp} = 
  sqrt(abs_float(endp.x -. startp.x)**2. +. abs_float(endp.y -. startp.y)**2.)

let print_point {x; y} =
  Printf.printf "(%g, %g)" x y


(* A.2 SICP 2.3 *)
type rectangle = {ll:point ; ur:point}

let rectangle_lower_segment {ll; ur} =
  make_segment ll (make_point (get_x ur) (get_y ll))

let rectangle_upper_segment {ll; ur} = 
  make_segment (make_point (get_x ll) (get_y ur)) ur

let rectangle_left_segment {ll; ur} = 
  make_segment (make_point (get_x ll) (get_y ur)) ll

let rectangle_right_segment {ll; ur} = 
  make_segment (make_point (get_x ur) (get_y ll)) ur

let rectangle_perimeter rect =
  segment_length (rectangle_lower_segment rect) +. 
  segment_length (rectangle_upper_segment rect) +.
  segment_length (rectangle_left_segment rect) +.
  segment_length (rectangle_right_segment rect)

let rectangle_area rect =
  segment_length (rectangle_lower_segment rect) *.
  segment_length (rectangle_right_segment rect)

type rectangle2 = {lx:float; ly:float; ux:float; uy:float}

let rectangle_lower_segment2 {lx=lx; ly=ly; ux=ux; _} =
  make_segment (make_point lx ly) (make_point ux ly)

let rectangle_upper_segment2 {lx=lx; ux=ux; uy=uy; _} = 
  make_segment (make_point lx uy) (make_point ux uy)

let rectangle_left_segment2 {lx=lx; ly=ly; uy=uy; _} = 
  make_segment (make_point lx ly) (make_point lx uy)

let rectangle_right_segment2 {ly=ly; ux=ux; uy=uy; _} = 
  make_segment (make_point ux uy) (make_point ux ly)

let rectangle_perimeter2 rect =
  segment_length (rectangle_lower_segment2 rect) +. 
  segment_length (rectangle_upper_segment2 rect) +.
  segment_length (rectangle_left_segment2 rect) +.
  segment_length (rectangle_right_segment2 rect)

let rectangle_area2 rect = 
  segment_length (rectangle_lower_segment2 rect) *.
  segment_length (rectangle_right_segment2 rect)

let make_rectangle ll ur =
  {ll = ll; ur = ur}

let make_rectangle2 lx ly ux uy = 
  {lx = lx; ly = ly; ux = ux; uy = uy}


(* A.3 SICP 2.4*) 
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(*
  Evaluation of first (make_pair x y)

  Evaluate first (make_pair x y)
    - Evaluate make_pair x y
      - desugar make_pair into
        let make_pair x y = fun m -> m x y
      - apply make_pair to x y

  Evaluate first (fun m -> m x y)
    - Desugar first to let first z = z (fun x y -> x)
  
    - Evaluate fun z -> z (fun x y -> x) (fun m -> m x y)
      - Substitute (fun m -> m x y) in for z
        (fun m -> m x y) (fun x y -> x)
      - Apply (fun x y -> x) to (fun m -> m x y) by substituting it in for m
        (fun x y -> x) x y
      - Apply x y to (fun x y -> x) 
        Return: x

  So, we see that it always returns the first value
*)

(*
  Evaluation of second (make_pair 1 2)

  Evaluate second (make_pair 1 2)
    - Evaluate make_pair 1 2
      - Evaluate 1 -> 1
      - Evaluate 2 -> 2
      - Evaluate make_pair
        - let make_pair x y = fun m -> m x y
        - Desugar this to
          fun x y -> fun m -> m x y
        - Apply 1 and 2 to (fun x y -> fun m -> m x y)
          - fun 1 2 -> fun m -> m 1 2
          - Return: fun m -> m 1 2

    - Evaluate second (fun m -> m 1 2)
      - Evaluate second
        - let second z = z (fun x y -> y)
        - Desugar this to
          fun z -> z (fun x y -> y)
        - Evaluate fun z -> z (fun x y -> y) (fun m -> m 1 2)
          - Apply (fun m -> m 1 2) to fun z -> z (fun x y -> y)
            by substituting it in for z
            (fun m -> m 1 2) (fun x y -> y)
          - Apply (fun x y -> y) to (fun m -> m 1 2) by substituting
            it in for m
            (fun x y -> y) 1 2
            - Evaluate 1 -> 1
            - Evaluate 2 -> 2
            - Apply 1 2 to (fun x y -> y) by substituting 1 for x
              and 2 for y
              fun 1 2 -> 2
              - Return: 2         
*)

(* A.4 SICP 2.5 *)
let rec pow base exp =
  match exp with
  | 0 -> 1
  | _ -> base * pow base (exp - 1)

let rec int_log base num =
  match num with
  | 0 -> 0
  | x when x mod base != 0 -> 0
  | _ -> 1 + int_log base (num / base)

let make_pairi x y = (pow 2 x) * (pow 3 y)
let firsti x = int_log 2 x
let secondi y =  int_log 3 y


(* A.5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev u =
  match u with
  | [] -> invalid_arg "Must have at least 1 digit"
  | () :: t -> t

let rec integer_to_unary n = 
  if n = 0 then zero
  else succ (integer_to_unary (n - 1))

let rec unary_to_integer u =
  if is_zero u then 0
  else 1 + unary_to_integer (prev u)

let unary_add u1 u2 = u1 @ u2

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' u =
  match u with
  | Zero -> invalid_arg "Must have at least 1 digit"
  | Succ s -> s

(* Yes, I had to change unary_add, since we are 
   no longer returning a list. *)
   let rec integer_to_unary' n = 
    if n = 0 then zero'
    else succ' (integer_to_unary' (n - 1))
  
  let rec unary_to_integer' u =
    if is_zero' u then 0
    else 1 + unary_to_integer' (prev' u)
  
  let rec unary_add' u1 u2 =
    match u1 with
    | Zero -> u2
    | Succ _ -> succ' (unary_add' (prev' u1) u2)


(* A.6 SICP 2.6 *)
(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. *)

let zerof = fun s -> fun z -> z
(* or equivalently: let zerof = fun s z -> z *)
(* or equivalently: let zerof s z = z *)

let add1 n = fun s -> fun z -> s (n s z)
(* or equivalently: let add1 n = fun s z -> s (n s z) *)
(* or equivalently: let add1 n s z = s (n s z) *)

(* How to get to one
   one is equal to adding 1 to zero. Substitute zerof into n in add1.
   let one = fun s -> fun z -> s (zerof s z)
   let one = fun s -> fun z -> s ((fun s -> fun z -> z) s z)
   let one = fun s -> fun z -> s (z)
   let one = fun s -> fun z -> s z
*)
let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)

let church_to_integer c = c (fun x -> x + 1) 0;;


(* A.7 *)
(* 1. church_to_integer zerof
   val zerof : 'a -> 'b -> 'b
   val church_to_integer : ((int -> int) -> int -> 'c) -> 'c

   We look at the types of zerof and church_to_integer. Notice that
   church_to_integer takes ((int -> int) -> int -> 'c) as an input.
   So, we know that the type of zerof should somehow match this type.
   Starting from the right, we match 'b to 'c. Then we match the second
   'b to int. Thus, we know that 'b must be int type. This means that
   'c must be int type as well, since its type should match 'b. And
   now we match 'a to (int -> int). Since 'c is int type, we see that
   church_to_integer will return int type, since it returns 'c type.

   2. church_to_integer one
   val one : ('a -> 'b) -> 'a -> 'b
   val church_to_integer : ((int -> int) -> int -> 'c) -> 'c

   Types of one should match the input type of church_to_integer, which
   is ((int -> int) -> int -> 'c). one has type ('a -> 'b) -> 'a -> 'b,
   so we see that 'a and 'b must be both int types by directly comparing
   these two. Thus, 'c must be int type as well. Since church_to_integer
   returns 'c type, it should therefore return an int.
*)


(* Part B *)
(* B.1 SICP 2.29 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch (Mobile (l, _)) = l
let right_branch (Mobile (_, r)) = r
let branch_length = function
  | Weight (l, _)
  | Structure (l, _) -> l

let branch_structure = function
  | Weight (_, w) -> `Weight w
  | Structure (_, m) -> `Structure m

let rec branch_weight1 = function
  | Weight (_, w) -> w
  | Structure (_, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) = branch_weight1 l + branch_weight1 r

let rec branch_weight2 b =
  let m = branch_structure b in
  match m with
  | `Weight w -> w
  | `Structure s -> total_weight2 s
and total_weight2 x = branch_weight2 (left_branch x) + branch_weight2 (right_branch x)

let rec is_balanced m =
  let l = left_branch m in
  let r = right_branch m in
  let comp_torq = 
    branch_length l * branch_weight2 l = 
    branch_length r * branch_weight2 r in
  let branch_is_balanced b = 
    match branch_structure b with
    | `Structure s -> is_balanced s
    | `Weight w -> true
  in
    comp_torq && branch_is_balanced l && branch_is_balanced r

type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = {left = l; right = r}
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l m = Branch' (l, Structure' m)
let left_branch' {left; _} = left
let right_branch' {right = r; _} = r
let branch_length' (Branch' (l, _)) = l
let branch_structure' = function
  | Branch' (_, Weight' w) -> `Weight w
  | Branch' (_, Structure' s) -> `Structure s

let rec branch_weight' b =
  let m = branch_structure' b in
  match m with
  | `Weight w -> w
  | `Structure s -> total_weight' s
and total_weight' x = branch_weight' (left_branch' x) + branch_weight' (right_branch' x)

let rec is_balanced' m =
  let l = left_branch' m in 
  let r = right_branch' m in
  let comp_torq' =
    branch_length' l * branch_weight' l =
    branch_length' r * branch_weight' r in
  let branch_is_balanced' b =
    match branch_structure' b with
    | `Structure s -> is_balanced' s
    | `Weight w -> true
  in
    comp_torq' && branch_is_balanced' l && branch_is_balanced' r


(* B.2 SICP 2.30 *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree (Tree elems) = 
  let rec square_elems elems =
    match elems with
    | [] -> []
    | Num n :: t -> Num (n * n) :: square_elems t
    | Sub s :: t -> Sub (square_tree s) :: square_elems t
  in
    Tree (square_elems elems)

let rec square_tree' (Tree elems) =
  let square_elem elem =
    match elem with
    | Num n -> Num (n * n) 
    | Sub s -> Sub (square_tree' s) in
  Tree (List.map square_elem elems)


(* B.3 SICP 2.31 *)
(* let square_tree'' tree = tree_map (fun n -> n * n) tree *)
let rec tree_map f (Tree elems) =
  let apply_f elem =
    match elem with
    | Num n -> Num (f n) 
    | Sub s -> Sub (tree_map f s) in
  Tree (List.map apply_f elems)


(* Part C *)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

(* C.1 *)
let rec simplify1 expr =
  match expr with
  | Add (a, Int 0) -> a
  | Add (Int 0, b) -> b
  | Add (Int a, Int b) -> Int (a + b)
  | Add (a, b) -> Add (simplify1 a, simplify1 b)
  | Mul (a, Int 0) -> Int 0
  | Mul (Int 0, b) -> Int 0
  | Mul (a, Int 1) -> a
  | Mul (Int 1, b) -> b
  | Mul (Int a, Int b) -> Int (a * b)
  | Mul (a, b) -> Mul (simplify1 a, simplify1 b)
  | Pow (a, 0) -> Int 1
  | Pow (a, 1) -> a
  | Pow (Int a, b) -> 
    let pow x y =
      match y with
      | 0 -> 1
      | 1 -> x
      | _ -> x * pow x (y - 1)
    in
      Int (pow a b)
  | Pow (a, b) -> Pow (simplify1 a, b)
  | _ -> expr

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e


(* C.2 *)
let rec deriv var expr =
  match expr with
  | Int _ -> Int 0 
  | Var v -> if v = var then Int 1 else Int 0
  | Add (Var a, Var b) -> Add (deriv var (Var a), deriv var (Var b))
  | Add (Var a, b) -> Add (deriv var (Var a), deriv var (simplify1 b))
  | Add (a, Var b) -> Add (deriv var (simplify1 a), deriv var (Var b))
  | Add (a, b) -> Add (deriv var (simplify1 a), deriv var (simplify1 b))
  | Mul (a ,b) -> Add (Mul (deriv var a, b), Mul (a, deriv var b))
  | Pow (a, b) -> Mul (Mul (Int b, Pow (a, b - 1)), deriv var a)

let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d