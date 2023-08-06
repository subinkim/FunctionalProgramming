(* CS 4 2022-23, Prof. Vanier
  * Lecture 16 (2/27/2023)
  * Live Coding
*)

(* The Y Combinator *)

(* how to get recursion without explicitly using recursion *)
(* we are going to develop a higher-order function called the Y combinator *)

(* a combinator is a special kind of function with no free variables *)
fun x -> x; (* I combinator *)
fun x -> fun y -> x; (* K combinator *)
fun f -> fun g -> fun x -> f x (g x); (* S combinator *)
(* notice that f, g, x are all formal arguments of the function *)
fun x -> (x + x); (* NOT a combinator because + is free *)

(* meanings of combinators do not depend at all on the context *)

(* fixpoints *)
(* f = almost_factorial f  *)
(* f is a function such that calling almost_factorial on f returns f itself *)

(* finding Y combinator *)
(* Y f = f (Y f) *)
(* writing this in OCaml *)
let rec y f = f (y f);
(* but this will not work, because it will fall into an infinite loop *)
(* this is because OCaml has eager evaluation, but it works with languages with lazy evaluation *)
(* so we can get around this by wrapping y f with fun expression *)
(*  Y f = f (Y f) to Y f = f (fun x -> (Y f) x)*)
let rec y f = f (fun x -> (y f) x);

(*
   let part_factorial self n =
    if n = 0
      then 1
  else n * self self (n - 1)
*)
(* we are passing in part_factorial as a self argument to itself *)
(* requires self self at the end because part_factorial requires two arguments *)

(*
   let part_factorial self =
    let f = self self in
      fun n ->
        if n = 0
          then 1
      else n * f (n - 1)
*)
(* this is going to put us into infinite loop of part_factorial *)
(* to resolve this, we need to wrap it with fun expression *)
(* 
let part_factorial self = 
  let f = fun x -> self self x in
    fun n ->
      if n = 0
        then 1 
    else n * f (n - 1) *)

(* 
let part_factorial self = 
  (fun f ->
    fun n ->
      if n = 0
        then 1 
      else n * f (n - 1))
  (fun x -> self self x)
      *) 
(* we can simplify this into: *)
(* 
let part_factorial self = 
  almost_factorial (fun x -> self self x)
*) 
(* 
let factorial  = 
  let z = fun self -> 
    almost_factorial (fun x -> self self x) in
  z z
*) 
(* 
let factorial  = 
  (fun z -> z z)
  (fun w -> 
    almost_factorial (fun x -> w w x))
*) 
(* 
let y f  = 
  (fun z -> z z)
  (fun w -> f (fun x -> w w x))
*) 

(* Applicative order Y combinator *)
(* let y = fun f -> (fun z -> z z)(fun w -> f (fun x -> w w x)) *)
(* run utop ~rectypes to run code like this but we don't want to use it *)

(* to check for types *)
type 'a mu = Roll of ('a mu -> 'a)
let unroll (Roll f) = f
(* 
let y f  = 
  (fun z -> unroll z z)
  (Roll (fun w -> f (fun x -> unroll w w x)))
*) 
(* this is technically not a combinator because of Roll/unroll*)
