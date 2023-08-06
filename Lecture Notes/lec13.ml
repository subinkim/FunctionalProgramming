(* CS 4 2022-23, Prof. Vanier
  * Lecture 13 (2/13/2023)
  * Live Coding
*)

(* The Environment Model *)

(* Binding *)
(* An association between a name and a value *)
(* e.g. let name = value in... *)

(* Frames *)
(* A collection of bindings, stored in some region of computer memory *)
(* Frames are used to look up the value associated with a name *)
(* Have an enclosing environment - parent frame *)

(* Environments *)
(* A linked chain of frames ending in the initial environment *)

(* Rules *)
(* 1. Non-recursive let *)
(*
let x = E in B (frame: F1)
- evaluates expression in E in current frame (F1) to get a value V
- creates a new frame F2 (whose parent is F1)
- creates a new binding in F2 between x and V
- evaluate B in the context of the frame F2 (new current env)
- once evaluation done, F1 becomes current environment again
*)

(* 1.a. Top-level let
   Top -level let expressions are also used to define values in modules.
   Other than this, it is equivalent to regular let/in expressions.
*)

(* 1.b. Non-recursive let/and
   let x = E1 and y = E2 in B (F1)
   - evaluates E1 to get V1 and E2 to get V2 in F1
   - creates F2
   - bindings: x to V1, y to V2
   - evaluate B in F2
   - current env switces to F1 again after evaluation
*)

(* 2. fun
   fun x -> B (F1)
   - a function is a pair consisting of : the coee of the fun expression
     and a pointer to the environment the function was created in
   - functions with multiple arguments are desugared into funcitons with
     one argument returning functions of only one argument
*)

(* 3. Applying a fun
   1. Construct a new frame
   2. Bind the formal parameters to the evaluated arguments of the function
      call in that new frame
   3. The new frame's parent is the environment associated with the function
      being applied
   4. Evaluate the function body in the context of the environment starting
      from the new frame
*)

(* 4. let rec
   let rec x = E in B (F1)
   - creates F2 (whose parent is F1)
   - creates a binding in F2 between x and a dummy value
   - evaluate E in F2 to get V
   - rebinds x to V
   - evaluates b in F2
   - evaluation done, F1 becomes current env again
   SHOULD NEVER EVALUATE X TO A DUMMY VALUE
*)

let sadd a b c =
  let r = ref 0 in
  begin
    r := !r + a;
    r := !r + b;
    r := !r + c;
    !r
  end
in
  sadd 1 2 3