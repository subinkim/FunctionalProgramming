(* CS 4 2022-23, Prof. Vanier
  * Lecture 18 (3/6/2023)
  * Live Coding
*)


(* Delimited Continuations *)

(* Continuations *)
(*
  3 + <| 5 * 2 |> - 1
  this continuation is equivalent to
  fun x -> 3 + x - 1   
*)
(* continuation is stored implicitly on the runtime stack *)
(*
   First-class continuations
   - Scheme's call/cc

   - e.g.
   3 * (call_cc (fun k -> 10)) + 2
   // doesn't use the continuation k
   // 3 * 10 + 2

   - e.g.
   3 * (call_cc (fun k -> k 10)) + 2
   // continuation k called with argument 10, call_cc aborts the entire computation
   // in its place, it calls fun x -> 3 * x + 2

   - e.g.
   let cont = ref (fun x -> x);;
   3 * (call_cc
      (fun k -> cont := k; 10)) + 2;;
   // stores the continuation k in the ref cell cont
   // given x, !cont x will compute 3 * x + 2, but it's not a function type
   //  - this is because it doesn't return a value to where it was called
   //  - it will return to where it was originally called, not where it's called
   //  - replaces the stack in effect with the exact same runtime stack that existed
   //    when the continuation was captured
   //  - e.g. 2 * !cont 42;; will ignore 2 *, because 2 * was not captured in 
   //    the original stack
   // replaces it with 10, computes the value
*)
(*
  Continuation vs Functions
  // continuations are inefficient - it needs to capture the entire runtime stack
  // this is dumb, because we probably only want the recent parts of the stack   
*)

(* Undelimited Continuations *)
(*
   - Continuations like call/cc
   - captures the entire runtime stack, all the way up to when the interpretors
     start running
*)

(* Delimited Continuations *)
(*
   - fix the efficiency problems by marking the furthest point back on the
     runtime stack that a continuation has to capture
   - this can be repesented as functions
   - OCaml has a library called delimcc
*)
(* new_prompt
   - creates a new "prompt"

   push_prompt
   - takes a prompt and a functino of type unit -> 'a (thunks)

   shift
   - takes a prompt and a function of one argument as its arguments
   - prompt = marks the limit of where on the runtime stack shift will have its effect
   - function = continuation
*)
(* e.g.
   let p = new_prompt ()
   let x = push_prompt p 
      (fun () -> 2 * shift p (fun k -> 100)) 
   // this continuation: fun x -> 2 * x
   // shift removes everything from the runtime stack up to the prompt
*)
(* e.g.
   let p = new_prompt ()
   let f () = shift p (fun k -> 100)
   let x = push_prompt p (fun () -> 2 * f ())
   // x will end up be 100
*)
(* Nondeterministic Computation *)