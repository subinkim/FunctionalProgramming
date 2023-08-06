(*
  2022-23 WI
  CS 4, Prof. Vanier
  Assignment 1
  Rachael Kim
*)

(* PART A*)

(* A.1 *)
(* 
1. 10
int = 10

2. 10.
float = 10.

3. 5 + 3 + 4
int = 12

4. 3.2 + 4.2
Error: This expression has type float but an expression 
was expected of type int

5. 3 +. 4
Error: This expression has type int but an expression 
was expected of type float

6. 3 + 4.2
Error: This expression has type float but an expression 
was expected of type int

7. 3 +. 4.2
Error: This expression has type int but an expression 
was expected of type float

8. 3.0 +. 4.2
float = 7.2

9. 9 - 3 - 1
int = 5

10. 9 - (3 - 1)
int = 7

11. let a = 3
val a : int = 3

12. let b = a + 1
val b : int = 4

13. a = b
bool = false

14. [1; 2; 3] = [1; 2; 3]
bool = true

15. [1; 2; 3] == [1; 2; 3]   
Is this the same as or different 
from the previous expression? Why?
bool = false
It's different because `==` returns true if two things 
are identical (physical equality), 
while `=` returns true if two things are structurally equal. 
This means that == checks whether two things have the same 
address in memory. Since these two arrays are stored in different parts
of the memory, this statement will return false.

16. [(1, 2, 3)]
(int * int * int) list = [(1, 2, 3)]

17. [1, 2, 3]   Explain why this gives the result it does. 
This is a nasty pitfall which highlights one of the less 
desirable features of OCaml’s syntax. (See the OCaml cheat sheet.)
(int * int * int) list = [(1, 2, 3)]
Even though we wanted a list, it instead gives a list of 3D tuple. 
This is because we separate the elements of tuples using commas, 
but for list, we separate the elements using semicolons.
This is why OCaml interpreted our input as a list of tuple instead 
of a list of int.

18. if b > a && b < a * b then b else a
int = 4

19. if b > a and b < a * b then b else a
Error: Syntax error

20. 2 + if b > a then b else a
int = 6

21. if b > a then b else a + 2 
Why is this different from the previous case?
int = 4
For the expression above, we add 2 to the result of the 
if-else statement, but this addition is
carried out before we evaluate the if-else, so regardless 
of whether b is bigger than a or not, we will add 2 to 
the value the if-else statement returns.
For this expression, however, + 2 is evaluated as part 
of else. So, if b is not bigger than a, then the 
expression will return a + 2. But since b is bigger 
than a, it will simply return the value of b.

22. (if b > a then b else a) + 2
int = 6

23. if b > a then b   This is not a syntax error. 
Why does this give a type error? Hint: What does 
OCaml assume if the else in an if/then/else form is left off?

Error: This expression has type int but an expression was 
expected of type unit because it is in the result of a 
conditional with no else branch. This gives a type error 
because the types of true value and the false value don't 
match.  Since this if-then statement doesn't have an 
else, if the condition is false (b <= a), the expression 
will simply evaluate to (), which has the type `unit` 
(as the error message tells us). And since type unit 
doesn't match the type of b, which is int, we will get 
a type error, instead of the syntax error.
*)


(* A.2 *)
let sum_of_squares_of_two_largest (x:int) (y:int) (z:int) = 
  if x >= z && y >= z then
    x * x + y * y
  else
    if x >= y && z >= y then
      x * x + z * z
    else
      y * y + z * z


(* A.3 *)
(* 
This function adds absolute value of b to a. So it checks
whether b is positive or not. If it is positive, it returns 
(+), and if it's less than or equal to 0, it returns (-).
By putting the parentheses around the operator, we are 
making it into a two-argument function, so the result of 
the if-then-else expression would take a and b as its 
argument and return whatever the result of that operation is.
*)


(* PART B *)

(* B.1 *)
(*  
1. With Applicative Evaluation Interpretor
It will be caught in an infinite loop because an interpretor 
with applicative evaluation evaluates arguments first before 
they are applied. Thus, with applicative evaluation,
when the interpretor encounters the statement test 0 (p ()), 
it will first evaluate 0 (which returns the value 0) and 
then try to evaluate p(). However, we know that p() returns p() by
the definition (let rec p () = p ()). Thus, it will be trapped in 
the infinite loop of calling p () forever, instead of being able 
to evaluate the if-then-else expression.

2. With Normal-Order Evaluation Interpretor
Interpretor with normal-order evaluation would interpret it 
as we designed, returning 0, because it will not evaluate 
arguments until the end, when we expand every single operations.
So, in this case, when the interpretor sees test 0 (p ()), 
it will not evaluate individual arguments, 0 and p (), until 
the end. Instead, it will go check the if condition, which requires
the evaluation of x, and then realise that the evaluated value of x 
is indeed equal to 0. So, without having to evaluate p () at all, 
it will simply move onto then statement return 0.
*)

(* B.2 *)
(*
This will give you stack overflow error, because it will be 
stuck in infinite recursion. Since new_if is a function, each 
argument will be evaluated before the procedure is applied.
So, when we run the new_if call within sqrt_iter, we will be 
evaluating the predicate, then_clause, and else_clause first 
before it goes on to check whether the predicate matches
true or false. And thus, it will be stuck in an infinite 
recursion of calling sqrt_iter due to
the second argument that's passed into new_if.
*)

(* B.3 *)
(*
1. Recursive or Iterative?
add_a is: recursive
add_b is: iterative
*)

(* 
2. add_a

let rec add_a a b =
  if a = 0
     then b
     else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Evaluate (add_a 2 5)
Evaluate 2 -> 2
Evaluate 5 -> 5
Evaluate add_a -> fun a b -> if ...
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else inc (add_a (dec 2) 5)
  evaluate (if 2 = 0 then 5 else inc (add_a (dec 2) 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        evaluate 2 -> 2
        evaluate 0 -> 0
        evaluate = -> [primitive function =]
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (inc (add_a (dec 2) 5))
        evaluate add_a (dec 2) 5
          evaluate (dec 2)
            evaluate 2 -> 2
            evaluate dec -> [primitive function dec]
            apply dec to 2 -> 1
      evaluate (inc (add_a 1 5))
        evaluate add_a 1 5
          Evaluate 1 -> 1
          Evaluate 5 -> 5
          Evaluate add_a -> fun a b -> if ...
            apply add_a to 1, 5 ->
              apply (fun a b -> if ...) to 1, 5
                substitute 1 for a, 5 for b in (if ...)
                  -> if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                evaluate (if 1 = 0 then 5 else inc (add_a (dec 1) 5))
                  if is a special form, so evaluate the first operand:
                    evaluate (1 = 0)
                      evaluate 1 -> 1
                      evaluate 0 -> 0
                      evaluate = -> [primitive function =]
                      apply = to 1, 0 -> false
                  first argument of if is false, so evaluate the third operand:
                    evaluate (inc (add_a (dec 1) 5))
                      evaluate add_a (dec 1) 5
                        evaluate (dec 1)
                          evaluate 1 -> 1
                          evaluate dec -> [primitive function dec]
                          apply dec to 1 -> 0
                            result: 0
                      evaluate (add_a 0 5)
                        evaluate 0 -> 0
                        evaluate 5 -> 5
                        evaluate add_a -> fun a b -> if ...
                          apply add_a to 0, 5 ->
                            apply (fun a b -> if ...) to 0, 5
                              substitute 0 for a, 5 for b in (if ...)
                                -> if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                              evaluate (if 0 = 0 then 5 else inc (add_a (dec 0) 5))
                                if is a special form, so evaluate the first operand:
                                  evaluate (0 = 0)
                                    evaluate 0 -> 0
                                    evaluate 0 -> 0
                                    evaluate = -> [primitive function =]
                                    apply = to 0, 0 ->true
                                first argument of if is true, so evaluate the second operand:
                                  evaluate 5 -> 5
                                  result: 5
                      evaluate (inc 5)
                        evaluate 5 -> 5
                        evaluate inc -> [primitive function inc]
                        apply inc to 5 -> 6
                    result: 6
          evaluate (inc 6)
            evaluate 6 -> 6
            evaluate inc -> [primitive function inc]
            apply inc to 6 -> 7
        result: 7
  return 7
*)

(*
3. add_b

let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
>>> evaluate 2 -> 2
>>> evaluate 5 -> 5
>>> evaluate add_b -> fun a b -> if...
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
>>>     evaluate 2 -> 2
>>>     evaluate 0 -> 0
>>>     evaluate = -> [primitive function =]
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
>>>       evaluate 2 -> 2
>>>       evaluate dec -> [primitive function dec]
          apply dec to 2 -> 1
        evaluate (inc 5)
>>>       evaluate 5 -> 5
>>>       evaluate inc -> [primitive function inc]       
          apply inc to 5 -> 6
>>>     evaluate add_b 1 6
>>>       evaluate add_b -> fun a b -> if...
          apply (fun a b -> if ...) to 1, 6
          substitute 1 for a, 6 for b in (if ...)
            -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
          evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
            if is a special form, so evaluate the first operand:
              evaluate (1 = 0)
>>>             evaluate 1 -> 1
>>>             evaluate 0 -> 0
>>>             evaluate = -> [primitive function =]
                apply = to 1, 0 -> false
            first argument of if is false, so evaluate the third operand:
              evaluate (add_b (dec 1) (inc 6))
                evaluate (dec 1)
>>>               evaluate 1 -> 1
>>>               evaluate dec -> [primitive function dec]
                  apply dec to 1 -> 0
                evaluate (inc 6)
>>>               evaluate 6 -> 6
>>>               evaluate inc -> [primitive function inc]
                  apply inc to 6 -> 7
>>>           evaluate add_b 0 7
>>>             evaluate add_b -> fun a b -> if...
                apply (fun a b -> if ...) to 0, 7
                substitute 0 for a, 7 for b in (if ...)
                  -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
                evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                  if is a special form, so evaluate the first operand:
                    evaluate (0 = 0)
>>>                   evaluate 0 -> 0
>>>                   evaluate 0 -> 0
>>>                   evaluate = -> [primitive function =]
                      apply = to 0, 0 -> true
                  first argument of if is true, so evaluate the second operand:
>>>                 evaluate 7 -> 7
                    result: 7

*)


(* PART C *)

(* C.1 *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

(* C.1.a *)
let e_term n = 1. /. float_of_int(factorial n)

(* C.1.b *)
let rec e_approximation n = 
  match n with
  | 0 -> 1.
  | n' when n' < 0 -> 1.0 /. e_approximation (-1 * n)
  | _ -> e_term n +. e_approximation (n - 1)

(* C.1.c *)
(* e_approximation 20 returns 2.71828182845904553
exp 1.0 returns 2.71828182845904509
the difference is only 4.44089209850062616e-16 *)

(* C.1.d *)
(* 
This returns float = infinity.
This is probably because the resulting number has too many digits for us to represent in a
float form, since each data type is limited in size. Floats are 64-bits in size in OCaml.
So, if the result of e_approximation 100 has too many digits to be represented with 64-bits,
an overflow will occur. OCaml doesn't raise error for overflows. Instead, it returns special
numbers like infinity, which is why we got infinity as our result, not an error.
*)


(* C.2 *)
let rec is_even n =
  match n with
  | 0 -> true
  | _ -> is_odd (n - 1)
and is_odd n =
  match n with
  | 0 -> false
  | _ -> is_even (n - 1)


(* C.3 *)
let rec f_rec n =
  if n < 3 then
    n
  else
    f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec (n - 3)

let rec f a b c i n =
  if i = n then
    a
  else
    f (a + 2 * b + 3 * c) a b (i + 1) n

let f_iter n =
  if n < 3 then
    n
  else
    f 2 1 0 2 n (* f(2) = 2, f(1) = 1, f(0) = 0 *)


(* C.4 *)
let rec pascal_coefficient x y =
  match x, y with
  | x, y when x = 0 || y = 0 || y > x -> failwith "invalid arguments"
  | x, y when y = x -> 1
  | 1, 1 -> 1
  | _, 1 -> 1
  | x, y -> pascal_coefficient (x - 1) (y - 1) + pascal_coefficient (x - 1) y