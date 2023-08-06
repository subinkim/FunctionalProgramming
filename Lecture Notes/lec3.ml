(* CS 4 2022-23, Prof. Vanier
  * Lecture 3 (1/11/2023)
  * Live Coding
*)

(* LINEAR RECURSIVE VS LINEAR ITERATIVE *)

(* Sum of first N integers *)
(* linear recursive process *)
let rec sum_integers n =
  if n = 0 then (* base case *)
    0
  else
    n + sum_integers(n - 1) (* recursion *)
;;

let rec sum_integers2 n =
  match n with
  | 0 -> 0
  | _ -> n + sum_integers2 (n-1) (* pending operations are stored in stack *)
;;

(* Two strategies to do this evaluation
   1. evaluate at the end - keep all the numbers as pending operations
    - linear time, linear space 
    - NORMAL-ORDER EVALUATION (LAZY EVALUATION/CALL-BY-NEED Evaluation) = fully expand and evaluate
   2. evaluate as we go
    - constant space
    - APPLICATIVE-ORDER EVALUATION (Call-By-Value Evaluation) = evaluate the arguments and then apply

*)
(* linear iterative process *)
let rec sum_iter sum i n = (* using a helper function *)
  if i = n then
    sum + n (* base case *)
  else
    sum_iter (sum + i) (i + 1) n (* recursive case *) 
      (* tail call optimisation (TCO) - doesn't grow the stack, because no pending operations *)
      (* for instance, you can't have this optimisation with python if you use recursion = tail call *)
      (* we don't lose any efficiency if we run it this way *)
;;

let sum_int n = 
  sum_iter 0 0 n
;;

(* Imperative vs Functional *)
(* imperative: variables whose values change, explicit loops *)
(* functional: variable values never change, use recursive helpers, less code *)