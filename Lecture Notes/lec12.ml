(* CS 4 2022-23, Prof. Vanier
  * Lecture 12 (2/8/2023)
  * Live Coding
*)

(* ref *)
let r = ref 42;;
let r' = 42;;
!r;; (* dereferencing a ref cell: get the value of ref *)
r.contents;; (* get the value of ref *)

(* ref cell is a special case of a record with mutable fields *)
type 'a ref = { mutable contents: 'a};;
r.contents <- 1001;; (* updating the ref value *)
(* mutable "variables" = mutable fields in records *)

let astate = ref 0;;
let accum0 x = astate := !astate + x;; (* := has a return value of unit *)
accum0 1;;
!astate;; (* returns 1 *)
accum0 1;;
!astate;; (* returns 2*)

let astate = ref 0
let accum x =
  begin
    astate := !astate + x;
    !astate
  end