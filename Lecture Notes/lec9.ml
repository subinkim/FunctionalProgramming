(* CS 4 2022-23, Prof. Vanier
  * Lecture 9 (1/30/2023)
  * Live Coding
*)

(* Data Abstraction *)

(* assuming we represent complex numbers as lists *)
let complex_add_lists c1 c2 =
  match (c1, c2) with
  | ([r1; i1], [r2;i2]) -> [r1 +. r2; i1 +. i2]
  | _ -> invalid_arg "Invalid arguments"
(* this is not a great way to represent complex numbers *)

(* tuples - can contain different types *)
let complex_add_tuples c1 c2 =
  match (c1, c2) with
  | ((r1, i1), (r2,i2)) -> (r1 +. r2, i1 +. i2)

  let complex_add_tuples c1 c2 =
    let (r1, i1) = c1 in (* when there is only one thing to match, *)
    let (r2,i2) = c2 in (* it is better to use let - in statement *)
    (r1 +. r2, i1 +. i2)

let complex_add_tuples (r1, i1) (r2, i2) =
  (r1 +. r2, i1 +. i2)

(* type alias *)
type complex = float * float

let complex_add_tuples ((r1, i1):complex) ((r2, i2):complex) =
  (r1 +. r2, i1 +. i2)

(* algebraic data types (ADT) *)
type complex = Complex of float * float
(* Complex is a constructor with type complex *)

let complex_add_adt c1 c2 =
  match (c1, c2) with
  | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1 +. r2, i1 +. i2)

let complex_add_adt (Complex (r1, i1)) (Complex (r2, i2)) = 
  Complex (r1 +. r2, i1 +. i2)

(* using records *)
type complex = { real: float; imag: float}

let complex_add_records { real=r1; imag=i1 } {real=r2; imag=i2} =
  {real = r1+.r2; imag=i1+.i2}
  (* order of data doesn't matter for records *)

(* OCaml actually has a Complex module *)
type t = { re: float; im: float }

(* Define an abstraction of a complex numbers *)
type complex = { real : float; imag : float }
let make_complex r i = { real = r; imag = i }
let get_real { real = r } = r (* leaving out part of the records is ok as well *)
let get_imag { imag = i } = i

let complex_add a b =
  make_complex
  (get_real a +. get_real b)
  (get_imag a +. get_imag b)

let complex_multiply a b =
  make_complex
    (get_real a *. get_real b -.
    get_imag a *. get_imag b)
    (get_real a *. get_imag b +.
    get_imag a *. get_real b)

(* custom operators *)
let ( +% ) = complex_add
let ( *% ) = complex_multiply

(* it is common to import these modules. Modules can be written in .mli file (interface file)
and the implementation of our codes will be written in .ml file   
*)

(* functors *)
(* functions take in a module corresponding to a data representation
   and output a module representing some useful operations on that representation
   functor = function on modules
*)