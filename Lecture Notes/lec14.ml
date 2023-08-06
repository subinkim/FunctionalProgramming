(* CS 4 2022-23, Prof. Vanier
  * Lecture 14 (2/15/2023)
  * Live Coding
*)

(* The Environmnt Model *)
(* Mutation and Message-Passing *)

(* Mutation *)
(*
   To handle mutation,
   - find the binding for the ref cell to be mutated
   - change the contents of the cell
*)

(* Trapped frames *)
(*
   Sometimes, the newly created frame (that usually goes away
   after evaluation) can't go away because it may still be in use
    = "Trapped Frame"
*)
let x = 10;;
let x = 20;;
(* three frames, first x is shadowed (inaccessible) *)

let x = 10;;
let f () = x;; (* binds to first x *)
let x = 20;; (* this will not change the value f is binded to *)
(* four frames - initial, first x, f, second x *)

let x = ref 10;;
let f() = !x;; (* encapsulating states that are only accessible through methods *)
let g y  = x := y;;
let x = 20;;
f ();; (* gives old value 10 *)
g 42;; (* changes the old value to 42 *)
f ();; (* returns the new value, 42 *)

(* e.g. Accumulator *)
let make_accum m =
  let sum = ref m in (* sum is external to fun, but internal (it's not a gloabl variable) *)
    fun n -> (* functions like this fun = "closures", because they close over the external binding sum *)
      begin
        sum := !sum + n;
        !sum
      end
(* all OCaml functions have an ability to close over *)
let acc = make_accum 0;;
acc 0;; (* returns 0 *)
acc 1;; (* returns 1 *)
acc 1;; (* returns 2 - adding to the internal reference value *)

(* Mutable fields in objects *)
let make_oo_accum m =
  object
    val mutable sum = m 
    method add n = sum <- sum + n
    method reset = sum <- 0
    method current = sum
  end

(* Object-oriented accumulator *)
let make_oo_accum m =
  let sum = ref m in
  object
    method add n = sum := !sum + n
    method reset = sum := 0
    method current = !sum
  end

let make_oo_accum2 m =
  let sum = ref m in
  object (self)
    method add n = self#current + n
    method reset = sum := 0
    method current = !sum
  end
