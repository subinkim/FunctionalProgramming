(*
   2022-23 Winter Term
   CS 4 Lab 3
    Rachael Kim
*)

(* Part A *)
(* A.1 *)
let rec last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [x] -> [x]
  | h :: t -> last_sublist t


(* A.2 *)
let reverse list =
  let rec iter lst res =
    match lst with
    | [] -> res
    | h :: t -> iter t (h :: res)
  in
    iter list []


(* A.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: (square_list t)

let square_list2 items = List.map (fun x -> (x * x)) items


(* A.4 *)
(*
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t ((h * h) :: answer)
  in iter items []
*)
(* 
This produces the answer list in the reverse order because every 
iteration, you are appending the new element at the front of the answer
list. You go through the original list from the head to the tail, so
this new list that's created will have head^2 appended first and tail^2
appended at the end. But since you append it the new element at the front
of the list, head^2 will be pushed all the way back to the end of the
answer list and tail^2 would come at the start of the list, and all the
elements in the middle would also have a reverse order.
*)
(*
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer :: (h * h))
  in iter items []
*)
(*
   This would not work because the arguments for :: constructor has been
   given in the wrong order. :: constructor creates a list from a single
   element and a list, but it takes the element on its left and the list
   on its right. In other words, it appends at element at the start of the
   list. In this case, we try to append the element at the end of
   the list, but that's against types of arguments the constructor requires
   on each side. 
*)

(* To make it work, we can change it to this: *)
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [h * h])
  in iter items []

(* 
   This is not the most efficient method because @ operator has O(n)
   time complexity. Thus, when this operator is used for every single
   element in the list, it will bring up the overall complexity to
   O(n^2). Better solution would be the following, since :: constructor
   only has O(1) time complexity and List.rev (which has O(n) complexity)
   will only be used one in this function call, at the very end.
*)
(* let square_list items =
  let rec iter things answer =
    match things with
      | [] -> List.rev answer
      | h :: t -> iter t (h * h :: answer)
  in iter items [] *)

(* A.5 *)
(* let count_negative_numbers list =
  List.length (List.filter (fun x -> x < 0) list) *)
(* More space efficient version of code *)
let count_negative_numbers list =
  let rec iter lst count =
    match lst with
    | [] -> count
    | h :: t ->
      if h < 0 then iter t (count + 1)
      else iter t count
  in
    iter list 0


(* A.6 *)
let power_of_two_list n = 
  let rec iter x next res =
    match x with
    | x when x = n -> List.rev res
    | _ -> iter (x + 1) (2 * next) (next :: res)
  in
    iter 0 1 []


(* A.7 *)
let prefix_sum lst =
  let rec iter lst sum res =
    match lst with
    | [] -> List.rev res
    | h :: t -> 
      let sum' = h + sum in
      iter t sum' (sum' :: res)
  in
    iter lst 0 []


(* A.8 *)
let deep_reverse list =
  let rec iter lst res =
    match lst with
    | [] -> res
    | h :: t -> iter t (reverse h :: res)
  in
    iter list []


(* A.9 *)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let deep_reverse_nested nlist =
  let rec iter list res =
    match list with
    | [] -> res
    | (Value _ as h) :: t -> iter t (h :: res)
    | List l :: t -> iter t (List (iter l []) :: res)
  in 
    match nlist with
    | Value _ -> nlist
    | List lst -> List (iter lst [])


(* Part B *)
let rec filter predicate sequence =
  match sequence with
    | [] -> []
    | h :: t when predicate h -> h :: filter predicate t
    | _ :: t -> filter predicate t

(* B.1 *)
let rec quicksort cmp list =
  match list with
  | [] -> []
  | pivot :: t ->
    let small = filter (fun x -> cmp x pivot) t in
    let big = filter (fun x -> cmp x pivot = false) t in
    (quicksort cmp small @ [pivot]) @ (quicksort cmp big)


(* B.2 *)
(* 
Quicksort is an instance of generative recursion not structural
recursion, because the recursion call is made on the data that we got by
putting the original data through some computation. So, instead of
using the tail of the list, we get two sublists out of the original list
by comparing every element of the tail to the pivot, and we call quicksort
on each of these. This makes it generative recursion not structural.
*)


(* B.3 *)
(*
   Ben's version of merge sort gives stack overflow error. This is because
   Ben's pattern matching statements have no case for singleton lists.
   Instead, those lists will be dealt with the general case (_ case), which
   tries to compute the odd and the even halves of the list. So, we will get
   an empty even list and an odd list that is identical to the given
   singleton list. Then it calls merge_sort on these two lists, but calling
   merge_sort on odd_half would put our algorithm in an infinite loop
   because this singleton list will be passed into merge_sort and go
   through the exact same process infinite amount of time. Thus, we will
   eventually run out of stack space, and the program will return stack
   overflow during evaluation error.
*)


(* B.4 *)
let rec insert_in_order cmp new_result a_list =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order cmp new_result t

let rec insertion_sort cmp a_list =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order cmp h (insertion_sort cmp t)


(* 
This represents structural recursion because we are not doing any
computations on a_list to divide it up into sublists. Instead, we are
simply returning a sublist of it by taking off the first element.
*)


(* Part C *)
(* C.1 *)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun x -> h :: x) rest)
(*
   The function first checks the argument passed in and pattern
   matches on it. If the provided list is empty, then the function
   returns a list of empty list. This is because an empty set
   is a subset of any set.

   When the list is not empty, it pattern matches to h :: t.
   With this method, we get the full list of subsets by appending
   a list of subsets without h element and a list of subsets with h
   as its element. The first list is calculated by calling subsets
   on t. The second list is calculated by adding h to every single
   element in the first list using List.map(). @ operator appends the
   two lists together.
   
   So, for instance, when we are given a list [1;2;3], we will first
   calculate all the subets of [2;3], which will be [[], [2], [3], [2,3]]
   Then we will call List.map on every single element of rest and append 1.
   We will get [[1], [1, 2], [1, 3], [1, 2, 3]]. The new elements will then
   be added to the rest list, which contains all the subsets of t. So now
   we have a complete list of subsets.
*)


(* C.2 *)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> r + 1) 0 sequence


(* C.3 *)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)


(* C.4 *)
let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h1::t1, h2::t2) -> (f h1 h2) :: map2 f t1 t2

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun x -> dot_product x v) m

let transpose mat = accumulate_n (fun x r -> x :: r) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (fun r -> matrix_times_vector cols r) m