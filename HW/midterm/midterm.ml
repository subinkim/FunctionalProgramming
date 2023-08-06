(* name: Rachael Kim *)
(* email: subinkim@caltech.edu *)

(* Part A *)
(* A.1 *)
(*
let f n =
  let rec aux n r =
    match n with
      | 0 -> r
      | _ -> aux (n - 1) (n + 1000 * n * r)
  in
    aux n 0
*)
(*
   1. Worst-case asymptotic time complexity with respect to n
   O(n)
   2. Reason
   For this function f, we will need to iterate aux until n reaches
   0. Since we only decrement n by 1 each time, we would need to iterate
   n times, which gives us the time complexity of O(n).
*)


(* A.2 *)
(*
let rec bounce m n =
  if m = n
    then n
    else
      if m < n
        then m + bounce (m + 1) n
        else n + bounce (m - 1) n
*)
(*
   1. Worst-case asymptotic time complexity with respect to m and n
   O(abs|m-n| + 1)
   2. Reason
   The function bounce will need to recurse for abs |m - n| times until
   m = n. And once m = n, we will have 1 extra step to take care of 
   m = n case. So, the worst case time complexity will be O(abs |m-n| + 1).
*)


(* A.3 *)
(*
let rec trib n =
  match n with
    | 0 -> 0
    | 1 -> 0
    | 2 -> 1
    | _ -> trib (n - 1) + trib (n - 2) + trib (n - 3)
*)
(*
   1. Worst-case asymptotic time complexity with respect to n
   O(3^n)
   2. Reason
   The time complexity will be 3^n. Each recursion of trib will generate 3
   extra recursive steps, as it makes three calls to trib, until n = 2, 1,
   or 0, and since we decrement n by 1 in each recursion, it will take us 
*)


(* A.4 *)
(*
let rec weird n =
  match () with
    | _ when n < 1 -> failwith "bad"
    | _ when n = 1 -> 1
    | _ when n mod 3 = 0 -> weird (n / 3)
    | _ when n mod 2 = 0 -> weird (n / 2)
    | _ -> weird (n - 1)
*)
(*
   1. Worst-case asymptotic time complexity with respect to n
   O(log n)
   2. Reason
   The worse-cast asymptotic time complexity will be achived in the case
   where n is not divisible by 3 or 2, nor less than or equal to 1. Then
   we would need to subtract 1 from n. When we subtract 1 from n, n must be
   divisible by 2, since n mod 2 is either 0 or 1. And then we can imagine
   these two steps repeatedly happening until n reaches 1. So, the worst-
   case time complexity will be 2 * log base 2 of x. However, following the 
   reasoning learnt in class, this expression, in big O notation, will be
   equal to O(log n). Thus, the worse-case asymptotic time complexity is
   O(log n).
   We can also infer this from the division operations in the weird function.
   Division is a good indicator of O(log n) time complexity.
*)


(* A.5 *)
(*
let rec f n =
  match () with
    | _ when n < 1 -> failwith "bad"
    | _ when n = 1 -> 1
    | _ when n mod 2 = 0 -> f (n / 2)
    | _ -> f (3 * n + 1)
*)
(*
   What aspect or aspects of this function makes it impossible to analyze by 
   the methods we’ve shown you in this course?
   
   It is impossible to analyze the time complexity of this function or even
   decide whether this function will ever terminate, because of the fact
   that the value of n oscillates during the execution of this function.
   For instance, starting with 47, we will need to call f (3 * 47 + 1),
   then f 71, then f 214, then f 107, and so on. In the worst-case scenario,
   we would need to go through f (3 * n + 1) every other step, and thus,
   increase the value of n at a faster rate than the rate at which f (n / 2)
   operation reduces the value of n. Thus, it gets difficult to determine,
   even with numbers as small as 47, how many steps it will require to
   complete the function operations. Thus, this aspect of the function makes
   it impossible to analyze by the methods we have learnt in this class.
*)


(* Part B *)
(* B.1 *)
(* B.1.a *)
let split3 lst = 
  let rec iter i list res1 res2 res3 =
    match list with
    | [] -> (List.rev res1, List.rev res2, List.rev res3)
    | h::t ->
      match i mod 3 with
      | 0 -> iter (i + 1) t (h :: res1) res2 res3
      | 1 -> iter (i + 1) t res1 (h :: res2) res3
      | _ -> iter (i + 1) t res1 res2 (h :: res3)
  in
    iter 0 lst [] [] []


(* B.1.b *)
let merge3 lst1 lst2 lst3 =
  let rec merge list1 list2 =
    match list1, list2 with
    | [], _ -> list2
    | _, [] -> list1
    | h1::t1, h2::t2 ->
      if h1 <= h2 then h1 :: merge t1 list2 else h2 :: merge list1 t2
  in
    merge (merge lst1 lst2) lst3


(* B.1.c *)
let rec merge_sort3 lst =
  match lst with
  | [] -> []
  | [_] -> lst
  | _ ->
    let (lst1, lst2, lst3) = split3 lst in
    merge3 (merge_sort3 lst1) (merge_sort3 lst2) (merge_sort3 lst3)


(* B.2 *)
(* B.2.a *) (* CHECK THIS *)
let smallest_index list =
  match list with
  | [] -> invalid_arg "smallest_index: not enough elements"
  | h :: t ->
    let rec smallest_value lst value counter index =
      match lst with
      | [] -> index
      | h::t ->
        if h < value then smallest_value t h (counter + 1) counter
        else smallest_value t value (counter + 1) index
    in
      smallest_value t h 1 0


(* B.2.b *)
let flip_n n lst =
  let rec iter n lst res =
    match n, lst with
    | 0, _ -> res @ lst
    | _, [] -> invalid_arg "flip_n: not enough elements"
    | _, h::t -> iter (n - 1) t (h::res)
  in
    iter n lst []


(* B.2.c *)
let block_sort1 lst =
  match lst with
  | [] -> []
  | _ -> flip_n (smallest_index lst + 1) lst
    

(* B.2.d *)
let rec block_sort_r lst =
  match block_sort1 lst with
  | [] -> []
  | h::t -> h :: block_sort_r t


let block_sort_i lst =
  let rec iter lst res =
    match block_sort1 lst with
    | [] -> List.rev res
    | h::t -> iter t (h :: res)
  in
    iter lst []


(* B.3 *)
(* B.3.a *)
let linrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      let (split1, split2) = split x in
      combine split1 (f split2)
  in f
 

(* B.3.b *)
let insert_r item =
  let is_base lst = lst = [] || item <= List.hd lst in
  let on_base lst = item :: lst in
  let split lst = (List.hd lst, List.tl lst) in
  let combine first rest_after_rec = first :: rest_after_rec in
    linrec is_base on_base split combine


(* B.3.c *)
let insertion_sort =
  let is_base lst = lst = [] in
  let on_base _ = [] in
  let split lst = (List.hd lst, List.tl lst) in
  let combine first rest_after_rec = insert_r first rest_after_rec in
    linrec is_base on_base split combine


(* B.4 *)
(* B.4.a *)
let binrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      let (res1, res2, res3) = split x in
      combine res1 (f res2) (f res3)
  in f 


(* B.4.b *)
let quicksort =
  let is_base lst = lst = [] in
  let on_base _ = [] in
  let split lst =
    match lst with
      | [] -> invalid_arg "quicksort: can't split"
      | h :: t -> 
        let comp value = fun x -> x < h = value in
        let lt = List.filter (comp true) t in
        let ge = List.filter (comp false) t in
        (h, lt, ge)
  in
    let combine pivot lt ge = lt @ (pivot :: ge) in
      binrec is_base on_base split combine

  
(* B.5 *)
(* B.5.a *)
let tailrec is_base on_base next =
  let rec f inputs =
    if is_base inputs then
      on_base inputs
    else
      f (next inputs)
  in f


(* B.5.b *)
let insert_i item lst =
  let is_base (_, rest) = rest = [] || item <= List.hd rest in
  let on_base (prev, rest) = List.rev prev @ (item :: rest) in
  let next (prev, rest) = (List.hd rest :: prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)


(* B.5.c *)
let insertion_sort_i lst =
  let is_base (_, rest) = rest = [] in
  let on_base (prev, _) = prev in
  let next (prev, rest) = (insert_i (List.hd rest) prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)


(* Part C *)
type tree =
  | Leaf
  | Node of int * int * tree * tree   (* level, value, left/right subtrees *)


(* C.1 *)
let rec member item tree =
  match tree with
  | Leaf -> false
  | Node (_, v, left, right) ->
    if v = item then true
    else (member item left) || (member item right)


(* C.2 *)
let level = function
  | Leaf -> 0
  | Node (lvl, _, _, _) -> lvl

let skew tree =
  match tree with
  | Node (lvl, v, Node (lvl2, v2, left2, right2), right) when lvl = lvl2 ->
     Node (lvl2, v2, left2, Node (lvl, v, right2, right))
  | Leaf -> tree
  | Node (_, _, _, _) -> tree
    
let split tree =
  match tree with
  | Node (lvl, v, left, Node (lvl2, v2, left2, 
    Node (lvl3, v3, left3, right3))) when (lvl = lvl2) && (lvl2 = lvl3) ->
      Node(lvl2 + 1, v2, Node (lvl, v, left, left2), 
                         Node(lvl3, v3, left3, right3))
  | Leaf -> tree
  | Node (_, _, _, _) -> tree


(* C.3 *)
let rec insert item t =
  match t with
    | Leaf -> Node (1, item, Leaf, Leaf)
    | Node (_, v, _, _) when v = item -> t
    | Node (lvl, v, l, r) ->
      if item < v then split (skew (Node (lvl, v, insert item l, r)))
      else split (skew (Node(lvl, v, l, insert item r)))

(* let print_tree tree =
  let blanks n = String.make n ' ' in
  let rec aux tree indent =
    let ind = blanks indent in
      match tree with
        | Leaf -> Printf.printf "%sLeaf\n" ind
        | Node (d, v, l, r) ->
          begin
            Printf.printf "%sNode[(%d) [level %d]\n" ind v d;
            aux l (indent + 2);
            Printf.printf "%s  ----\n" ind;
            aux r (indent + 2);
            Printf.printf "%s]\n" ind;
          end
  in
    aux tree 0

let tree_of_list lst = List.fold_left (fun l i -> insert i l) Leaf lst *)