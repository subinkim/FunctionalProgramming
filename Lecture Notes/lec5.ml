(* CS 4 2022-23, Prof. Vanier
  * Lecture 5 (1/18/2023)
  * Live Coding
*)

(* Higher Order Functions *)
(* passing in function as an argument for another function *)
let rec rsum f m n = (* we want to sum f(m) to f(n), n >= m *)
  if m > n then
    0
  else
    f m + rsum f (m + 1) n
  ;;

let isum f m n =
  let rec iter f m n sum =
    if m > n then
      sum
    else
      iter f (m + 1) n (sum + f m)
  in
    iter f m n 0
;;

let isum f m n =
  let rec iter m sum = (* we get rid of f and n because they never change *)
    if m > n then
      sum
    else
      iter (m + 1) (sum + f m)
  in
    iter m 0
;;

(* Lambda Calculus *)
(* 
Variables: x
Functions of one argument: fun x -> x
Function applications: (fun x -> x) y
*)

let rec gsum f lo next hi = (* f = function, next = function that moves lo to hi *)
  if lo > hi then
    0
  else
    f lo + gsum f (next lo) next hi;; (* Generalize Increments *)

let step1 n = n + 1;;
gsum (fun i -> i) 5 step1 10;;

let sum2 f lo hi =
  gsum f lo (fun i -> i * 2) hi;;

(* Floating point version of gsum *)
let rec fgsum f lo next hi =
  if lo > hi then
    0.0
  else
    f lo +. fgsum f (next lo) next hi;;

let ifgsum f lo next hi =
  let rec iter lo sum = 
    if lo > hi then
      sum
    else
      iter (next lo) (sum +. f lo)
  in
    iter lo 0.0

(* Computing integrals *)
let integral f lo hi dx = 
  dx *. fgsum f lo (fun x -> x +. dx) hi;; (* could give stack overflow for very small dx *)

let integral2 f lo hi dx = 
  dx *. ifgsum f lo (fun x -> x +. dx) hi;;

(* We can also generalize the operator by making it another parameter *)