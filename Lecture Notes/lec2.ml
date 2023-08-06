(* CS 4 2022-23, Prof. Vanier
  * Lecture 2 (1/9/2023)
  * Live Coding
*)

(* OCaml Expressions *)
abs (-7);;
(* abs -7 *) (* Returns an error because here you are trying to
          subtract 7 from abs function *)
abs ~-7;;
abs 7 + abs 8;; (* Function application has very high precedence *)
3.4 +. 2.4;; (* Notice the period next to + *)

(* Operators into functions *)
2 + 2;;
(+) 2 2;; (* By putting parentheses around, you make it into 
           a 2-argument function *)

(* Primitive Functions = Functions that are built into OCaml 
  * e.g. +, -, /, *, abs 
  * They evaluate to the corresponding internal operator
  * + -> [primitive expression +]
  *)

(* Defining value for a local scope *)
let x = 2 + 3;;
let y = x + x in y + 42;;
(* y;; (* Gives an error because y was only defined for that one line *)
*)
(* fun expressions *)
fun x -> x + 2;;
(* Result of fun is always a fun*)
(* fun doesn't really do any evaluations *)
let g = fun x -> 2 + 2;; (* This is not going to return 4.
                          2 + 2 is only evaluated when g is called *)
let g x = 2 + 2;; (* Syntactic sugar *)

let rec sum n =
  if n = 1 then
    1
  else
    n + sum (n - 1)

let rec sum n = 
  match n with
  | 1 -> 1
  | n' -> n + sum (n' - 1)
;;

"hello" = "hello" ;; (* Returns true - structurally equal*)
"hello" == "hello"  ;; (* Returns false - not identical, they are stored in two separate memory spaces*)
true && false;;
true || false