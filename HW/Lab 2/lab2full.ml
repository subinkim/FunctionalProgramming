(* Lab 2 *)

(* A.1 *)
(*
let rec fib n =
  if n < 2
     then n
  else fib (n - 1) + fib (n - 2)

What is this function's space complexity, given that it uses applicative order evaluation?
*)
(*
  This function's space complexity will be O(n). We only consider the largest number of pending
  operations. Thus, we can solve this problem by drawing a tree that starts with fib n and maps
  all the recursive calls generated while evaluating this expression. The maximum depth of the
  tree is n, which is why our space complexity will be O(n).
  This is different from time complexity, because we are not measuring the amount of resources
  needed to evaluate ALL the expressions involved in fib n for space complexity. For time
  complexity, however, we are pretty much counting the total number of expressions we get while
  evaluating fib n, which is 2^n (since each fib call returns two other fib calls or two other
  expressions). Thus, in this example, space complexity would have the value of 
  O(log 2^n) = O(n).
*)

(* A.2 SICP 1.15 *)
let cube x = x *. x *. x
let p x = 3.0 *. x -. 4.0 *. cube x
let rec sine angle =
  if abs_float angle < 0.1
     then angle
     else p (sine (angle /. 3.0))

(*
  1. How many times is the function p applied when sine 12.15 is evaluated?
  2. What is the order of growth in space and number of steps used by the process generated by 
  the sine function when sine a is evaluated (as a function of a)? = space/time complexity
*)

(*
  1. It will be applied 5 times:
  sine 12.15
  p (sine 4.05)
  p (p (sine 1.34999999999999987))
  p (p (p (sine 0.449999999999999956)))
  p (p (p (p (sine 0.15))))
  p (p (p (p (p (sine 0.0499999999999999958)))))
  p (p (p (p (p 0.0499999999999999958))))
  ...

  2. Approaching this problem mathematically: first, notice that we are trying to find minimum
  value of n such that a / (3^n) <= 0.1, where a = angle. Then, a/0.1 <= 3^n, log (10a) <= n.
  Notice that n implies the largest number of pending operations (because all the p operations
  will be pending until the angle is equal to or goes below 0.1), and thus is our space
  complexity. Thus, our the growth in space would be O(log(angle)) (more accurately, 
  O(log(10*angle)), but we ignore coefficients).
  Now, we try to evaluate time complexity, which can be inferred through the following: 
  first, sine only have one recursive call in the else statement. Second, in the else statement,
  we are dividing angle by 3.0. This is a good indication that the function has logarithmic 
  time complexity. We can also prove this mathematically: notice that we would need 2n + 1
  steps to complete all the operations - n steps for all the recursive sine calls, 1 step to
  evaluate the last sine expression, and n steps for al the p calls. Since n is logarithmic,
  we know that our time complexity would also be logarithmic. Thus, the time complexity will be
  O(log(angle)) as well (or 2*O(log(10*angle))+1).
*)


(* A.3 SICP 1.16 *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    if n = 0 then 1
    else if is_even n then square (fast_expt b (n / 2))
    else b * fast_expt b (n - 1)

(* A.3.a *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
      | 0 -> 1
      | n when is_even n -> square (fast_expt b (n / 2))
      | _ -> b * fast_expt b (n - 1)

(* A.3.b *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter a b n =
    match n with
    | 0 -> a
    | n when is_even n -> iter a (square b) (n / 2)
    | _ -> iter (a * b) b (n - 1)
  in
    iter 1 b n


(* A.4 SICP 1.17 *)
(* Performing exponentiation by multiplication *)
let rec expt a b =
  if b = 0
     then 1
     else a * expt a (b - 1)

(* Performing exponentiation by addition *)
let rec mult a b =
  if b = 0
      then 0
      else a + mult a (b - 1)

(* Design a function that does mult by using addition, halve, and double *)
let rec fast_mult a b =
  let double m = m * 2 in
  let halve m = m / 2 in
  match b with
  | 0 -> 0
  | b when b mod 2 = 0 -> fast_mult (double a) (halve b)
  | _ -> a + fast_mult a (b - 1)


(* A.5 *)
let ifast_mult a b =
  let double m = m * 2 in
  let halve m = m / 2 in
  let rec iter a b n =
    match b with
    | 0 -> n
    | b when b mod 2 = 0 -> iter (double a) (halve b) n
    | _ -> iter a (b - 1) (n + a)
  in
    iter a b 0


(* A.6 *)
let rec foo f n =
  if n <= 1
     then f 0
     else foo f (n / 2) + foo f (n / 2)
(*
   If we assume that the function f can compute its result in constant time and constant space, 
   what are the (worst-case) time and space complexities of the function foo? Justify your 
   answer. (It doesn’t have to be a full mathematical proof, but it should be a convincing 
   argument.) Assume that the integer input n is always non-negative, and assume the usual 
   applicative-order evaluation rule.
*)
(*
   The time complexity of this function will be n. Having two recursive statements within
   the function would make the time complexity 2^n as each call of foo will lead to two
   calls of foo, but we also notice that the value of n gets halved in each call.
   Thus, this makes the overall time complexity O(2^(log n)) = O(n).

   The space complexity of this function will be log n. For space complexity, we want to
   look at the depth of the tree recursively created by this function. The depth of this
   tree will be log n, as we are dividing n by 2 in each recursion (we discusssed this 
   earlier in time complexity as well).
*)


(* A.7 *)
let fib n =
  let rec last_two n =
    if n < 1
      then (0, 1)
      else
        let (p0, p1) = last_two (n - 1) in
          (p1, p0 + p1)
  in
    fst (last_two n)
(*
   1. What kind of process does this function represent (linear recursive, 
   linear iterative, tree recursive etc.) and why?

   2. What is the space and time complexity of this function with respect
    to its argument n?
*)
(*
   1. Linear recursive - this is because we are recursing from n all the way down to 0,
   while decrementing n by 1 in each recursion (so it's linear). We also know that this
   cannot be iterative, because every time we call last_two recursively, there will be
   pending operations that need to be taken care of. Thus, it must be linear recursive.

   2. O(n) for both - because we have n recursive steps for fib, and each recursive call
   only creates one another recursive call.
*)

(* Part B *)
(* B.1.a *)
let x = 20
and y = 2 * 4
in x * (2 + y)
(*
   (fun x y -> x * (2 + y)) 20 (2 * 4)
*)

(* B.1.b *)
let a = 1.0
and b = 20.0
and c = 3.0
in sqrt (b *. b -. 4.0 *. a *. c)
(*
   (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
*)

(* B.1.c *)
let x = 1 in
let y = 2 in
let z = 3 in
  x * y * z
(*
   (fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1
*)

(* B.1.d *)
let x = 1 in
let x = 2 in
let x = 3 in
  x * x * x

(*
   (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
   Returns 27 becasue three x's in x * x * x will correspond to the
   x in the third fun x.
*)


(* B.2 *)
let x = 2 * 10
and y = 3 + 4
in
  let y = 14 in
  let z = 22 in
    x * y * z

(*
let x = 2 * 10
and y = 3 + 4
in
  let y = 14 in
  let z = 22 in
    x * y * z

Desugar this into: 
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)


Evaluate 2 * 10
  Evaluate 2 -> 2
  Evaluate * -> [primitive function *]
  Evaluate 10 -> 10
  Apply * to 2 and 10 -> 20
Evaluate 3 + 4
  Evaluate 3 -> 3
  Evaluate 4 -> 4
  Evaluate + -> [primitive function +]
  Apply + to 3 and 4 -> 7
Apply (fun x y -> ...) to 20, 7
  Substitute 20 for x
  Substitute 7 for y

  Evaluate 14 -> 14
  Apply (fun y -> ...) to 14
    Substitute 14 for y
    Apply (fun z -> 20 * 7 * z) to 22
      Evaluate 22 -> 22
      Substitute 22 for z
      Evaluate 20 * 7 * 22 -> 6160
*)

(* B.3 *)
let x = 10
and y = x * 2
and z = y + 3
in x + y + z
(*
   This will desugar into:
   (fun x y z -> x + y + z) 10 (x * 2) (y + 3)
   But as you can see, x in x * 2 and y in y + 3 are not within the same scope as the
   function parameters x and y. In other words, x in x * 2 is not the same as x in the
   function definition, and y in y + 3 is not the same as y in the function definition.
   Also, when we evaluate this expression, we evaluate the arguments before we apply
   them to the function. Thus, we will get an error that says "Unbound value x", because 
   when we pass in x * 2 for y, we are technically passing in something we have never 
   declared nor assigned value for.
*)


(* Part C *)
open Num
let ni = num_of_int     (* convert int -> num *)

(* C.1 SICP Problem 1.30 *)
(* linear recursive *)
let rec sum term a next b =
  if a >/ b
     then (ni 0)
     else term a +/ (sum term (next a) next b)

(* 
   term is a function of one argument which generates the current term in a sequence 
   given a sequence value, while next is a function of one argument which generates the 
   next value in the sequence.
*)
(* Do it in a linear iterative way: *)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (result +/ term a)
  in
    iter a (ni 0)


(* C.2 SICP 1.31 *)
let rec product_rec term a next b =
  if a >/ b
    then (ni 1)
  else term a */ (product_rec term (next a) next b)

let product_iter term a next b =
  let rec iter a result =
    if a >/ b
      then result
    else iter (next a) (result */ term a)
  in
    iter a (ni 1)

let step1 n = n +/ ni 1

let self n = n

let factorial_rec n =
  product_rec self (ni 1) step1 n

let factorial_iter n =
  product_iter self (ni 1) step1 n

let pi_product n =
  let term_def n = (ni 4 */ square_num(n)) // (ni 4 */ square_num(n) -/ ni 1) in
  ni 2 */ (product_rec term_def (ni 1) step1 n)

let pi_approx =
  float_of_num(pi_product (ni 1000))


(* C.3 SICP 1.32 *)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
    then null_value
  else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a >/ b
      then result
    else iter (next a) (combiner result (term a))
  in
    iter a null_value

let sum term a next b =
  accumulate_rec (+/) (ni 0) term a next b

let product term a next b =
  accumulate_rec ( */ ) (ni 1) term a next b


(* C.4 SICP 1.42 *)
let compose f g =
  fun x -> f (g x)


(* C.5 SICP 1.43 *)
(* let repeated f n =
  let rec iter n x =
    if n == 0
      then x
    else
      iter (n - 1) (f x)
  in
    fun x -> iter n x *)

let rec repeated f n =
  if n == 0 then
    fun x -> x
  else 
    compose f (repeated f (n - 1))

(* C.6 SICP 1.44 *)
let smooth dx f = 
  fun x -> ((f (x -. dx) +. f x +. f (x +. dx)) /. 3.0)

let nsmoothed dx f n =
  (repeated (smooth dx) n) f


(* Part D *)
(* D.1 *)
let is_prime n =
  let rec checkFactors m = 
    float_of_int(m) > sqrt(float_of_int(n)) || (n mod m != 0 && checkFactors(m + 1))
  in
    n >= 2 && checkFactors 2
  
(* D.2 *)
let smallest_prime_factor n =
  if n < 2 || is_prime n then
    invalid_arg "Invalid function argument"
  else
    let rec iter p =
      if is_prime p && n mod p == 0 then
        p
      else
        iter (p + 1)
    in 
      iter 2
