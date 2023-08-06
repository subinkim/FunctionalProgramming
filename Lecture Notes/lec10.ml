(* CS 4 2022-23, Prof. Vanier
  * Lecture 10 (2/3/2023)
  * Live Coding
*)

(* Tagged Data *)

(* Polymorphic Variants *)
`Length 10;;
`Length "foobar"

let string_of_color v = 
  match v with
  | `Red -> "red"
  | `Green -> "green"

(* < in [< `Green | `Red] means "less than or equal to" *)
(* any datatype that consists of no more than the constructors 
    `Green and `Red is acceptable as input to this function *)

let color_of_string v =
  match v with
  | "red" -> `Red
  | "green" -> `Green
  | _ -> `Unknown

(* the return value of this function would be an acceptable input 
to another function that could accept at least the constructors 
`Green, `Red, `Unknownand possibly more (that's what the >means) *)

(* don't use this unless you know what you are doing *)
(* this type casts *)
Obj.magic
let int_of_string (i:int):string = Obj.magic i;;

(* using polymorphic variants to mimic units *)
`Meter 3.0;;

