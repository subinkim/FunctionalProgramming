(* CS 4 2022-23, Prof. Vanier
  * Lecture 4 (1/13/2023)
  * Live Coding
*)

let rec fib n = 
  if n < 2 then n else fib (n - 1) + fib (n - 2);; (* This gives repeated calculations *)
(* Why is it so slow?
   - a lot of unnecessary computations
   - exponential time, because T(fib) has exponential upper & lower bounds
  *)

let ifib n = 
  let rec iter i a b = (* internal function *)
    if i = 0 then
      a
    else
      iter (i - 1) b (a + b)
  in
    iter n 0 1;;

let rec fib_iter fnext f cnt = (* fnext, f, cnt are 'state variables' *)
  if cnt = 0
      then f
  else fib_iter (fnext + f) fnext (cnt - 1);; 
    
let ifib n =
  fib_iter 0 1 n;;

(*****************************)
(* Space and time complexity *)
(*****************************)

(* linear iterative *)
(* constant space - no pending operations because we use tail calls *)
(* ifib also has linear time *)

(* Tail Calls *)
(* most functional languages support tail calls, but most non-functional languages don't, e.g python, java *)
(* without tail call, you will eventually get stack overflow error, because due to all the pending operations
   you will run out of spaces on the stack *)

(* Memoization *)
(* make your program memorize values already computed so it doesn't have to recompute *)

(* Time complexity *)
(* if there's one recursive call *)
(* - consider which arguments control termination of function *)
(* - how fast they chang from one call to the next *)
(* - when we subtract a fixed amount in the recursive call = usually linear *)
(* - when we divide = usually logarithmic *)

(* Big O notation *)
(* if g(n) = 100 n^2 + 50n + 2000, O(n^2) *)
(* but this doesn't always accurately describe time complexity *)

(* Big Theta notation *)
(* bounded below & above *)
(* if g(n) = 100 n^2 + 50n + 2000, C1*n^2 <= g(n) <= C2*n^2 *)
(* g(n) is Big Theta (n^2) *)
(* fib is O(2^n) but theta(g^n) where g = golden ratio *)

(* Asymptotic complexity *)
(* we only care about the fastest growing term *)

let times10 n = (n + n + n + n + n) * 2;; (* time complexity = O(1) *)

let rec is_even n = (* time complexity = O(n) *)
  match n with
  | 0 -> true
  | -1 -> false
  | _ -> is_even (n - 2);;

let member v arr = (* time complexity = O(n) *)
  let len = Array.length arr in
  let rec iter i =
    if i >= len then
      false
    else if arr.(i) = v then
      true
    else
      iter (i + 1)
  in
    iter 0;;
