(* CS 4 2022-23, Prof. Vanier
  * Lecture 8 (1/27/2023)
  * Live Coding
*)

(* List Processing, Part 2 *)
let make_result x y = (x, y)
let get_x (x, _) = x
let get_y (_, y) = y

(* 1. Remove an element from the list with a given x value *)

(* Recursive approach *)
let rec remove_by_x_r lst x =
  match lst with
  | [] -> []
  | h :: t -> if get_x h = x then t else h :: remove_by_x_r t x (* no tail call, so high space complexity *)

let rec remove_by_x_r x lst =
  match lst with
  | [] -> []
  | h :: t -> if get_x h = x then t else h :: remove_by_x_r t x

let rec remove_by_x_r x = function (* shorter but could be confusing with functions with multiple arguments *)
  | [] -> []
  | h :: t -> if get_x h = x then t else h :: remove_by_x_r t x

(* Iterative approach *)
let remove_by_x_i lst x =
  let rec iter rest curr = 
    match rest with
    | [] -> List.rev curr
    | h :: t when get_x h = x -> List.rev curr @ t
    | h :: t -> iter t (h :: curr)
  in
    iter lst []

(* higher order function approach *)
let remove_by_x lst x =
  List.filter (fun r -> get_x r <> x) lst (* It will remove every value in the list with x value that is not the given x *)
(* filter returns all the elements in the list that satisfy the given condition *)

let remove_by_x lst x =
  List.rev 
  (List.fold_left (
    fun r h -> if get_x h = x then r else h :: r) 
    [] 
    lst)

(* 2. Insert a value into a list in order *)

(* Assuming it's in ascending order *)
let rec insert_in_order item lst = 
  match lst with
  | [] -> [item]
  | h :: t -> 
    if get_x item < get_x h 
      then item :: lst 
    else 
      h :: insert_in_order item t

(* Generalization: assuming it's in any order *)
let rec insert_in_order_by cmp item lst = 
  match lst with
  | [] -> [item]
  | h :: t -> 
      if cmp h item then
        item :: lst
      else
        h :: insert_in_order_by cmp item t

let insert_in_order2 item lst =
  insert_in_order_by
    (fun r1 r2 -> get_x r1 <= get_x r2)
    item
    lst

let insert_in_order3 =
  insert_in_order_by
    (fun r1 r2 -> get_x r1 <= get_x r2) (* Leaving it as a partial application of a function *)


(* 3. Insertion Sort *)
(* IDEA: peeling a value off of an original list and adding it to the new list *)
let insertion_sort cmp lst =
  let rec iter rest curr =
    match rest with
    | [] -> curr
    | h :: t -> iter t (insert_in_order_by cmp h curr)
  in
    iter lst [] 

let cmp_a x y = get_x x < get_x y


(* 4. Functional Merge Sort *)
(* IDEA: split list in half, sort halves, and merge the sorted halves in order *)
(* time complexity: O(Nlog(N)) - we split the list log(N) times and O(N) work for each sort *)
let rec merge_sort a_list cmp =
  match a_list with
  | []
  | [_] -> a_list
  | _ ->
    let rec odd_half a_list =
      match a_list with
      | [] -> []
      | [x] -> [x]
      | h :: _ :: t -> h :: odd_half t in
    let even_half a_list =
      match a_list with
      | [] -> []
      | _ :: t -> odd_half t in
    let eh = even_half a_list in
    let oh = odd_half a_list in
    let rec merge_in_order list1 list2 cmp =
      match (list1, list2) with
      | ([], _) -> list2 (* base case *)
      | (_, []) -> list1  (* base case *)
      | ...

(* Merge sort - Generative Recursion, Insertion sort - Structural Recursion *)