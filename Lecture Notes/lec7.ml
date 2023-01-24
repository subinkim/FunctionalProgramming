(* CS 4 2022-23, Prof. Vanier
  * Lecture 7 (1/23/2023)
  * Live Coding
*)

(* LISTS *)
(* type def *)
type listy = 
  | Nil (* empty list *)
  | Cons of int * listy (* takes two elements: head, tail *)

Cons (1, Nil);;
Cons (1, (Cons (2, (Cons (3, Nil)))));;

(* Creating lists of 'a = polymorphic type *)
type 'a listy = (* parametric polymorphism *)
  | Nil
  | Cons of 'a * 'a listy

[];; (* has 'a list type *)
:: (* a constructor that creates a NEW list by attaching a value to a list *)
1 :: [2];; (* will return [1; 2] *) (* note that lists are IMMUTABLE - so it returns a NEW list*)

(* Using modules *)
let lst = [1; 2; 3; 4];;
List.length ;;
List.hd lst;;
List.tl lst;;

open List;;
hd lst;;
tl lst;;

module L = List;;
L.hd lst;;

[1;2;3] @ [4;5;6] (* returns [1;2;3;4;5;6] *)

(* Pattern matching on lists *)
let rec sum_list lst = 
  match lst with
  |[] -> 0
  | h :: t -> h + sum_list t

let rec sum_list lst = function (* more concisely *)
  |[] -> 0
  | h :: t -> h + sum_list t

(* we cannot use @ in pattern matches because:
1. it's an operator
2/ not as well defined as :: constructor
   *)


let make_result x y = (x, y)
let get_x (x, _) = x
let get_y (_, y) = y

(* Structural recursion *)
let rec len lst =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + len t

(* min y at least 0.0 *)
let rec find_max_y_r results = 
  match results with
  | [] -> 0.0
  | h :: t -> max (get_y h) (find_max_y_r t)

let find_max_y_i results = 
  let rec iter rest ymax
    match rest with
    | [] -> ymax
    | h :: t -> iter t (max (get_y h) ymax) (* tail call - you can't do anything about the result the helper method returns. You just need to return it *)
  in
    iter results 0.0

(* without assumption *)
let find_max_y_i results = 
  let rec iter rest ymax
    match rest with
    | [] -> ymax
    | h :: t -> iter t (max (get_y h) ymax) (* tail call - you can't do anything about the result the helper method returns. You just need to return it *)
  in
    match results with
    | [] -> failwith "no results"
    | h :: t -> iter t (get_y h)
